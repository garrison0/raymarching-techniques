#version 300 es

precision highp float;

in vec2 vUv;
uniform vec2 uResolution;
uniform float uTime;

out vec4 o_FragColor;

float mod289(float x){return x - floor(x * (1.0 / 289.0)) * 289.0;}
vec4 mod289(vec4 x){return x - floor(x * (1.0 / 289.0)) * 289.0;}
vec4 perm(vec4 x){return mod289(((x * 34.0) + 1.0) * x);}

float noise(vec3 p){
    vec3 a = floor(p);
    vec3 d = p - a;
    d = d * d * (3.0 - 2.0 * d);

    vec4 b = a.xxyy + vec4(0.0, 1.0, 0.0, 1.0);
    vec4 k1 = perm(b.xyxy);
    vec4 k2 = perm(k1.xyxy + b.zzww);

    vec4 c = k2 + a.zzzz;
    vec4 k3 = perm(c);
    vec4 k4 = perm(c + 1.0);

    vec4 o1 = fract(k3 * (1.0 / 41.0));
    vec4 o2 = fract(k4 * (1.0 / 41.0));

    vec4 o3 = o2 * d.z + o1 * (1.0 - d.z);
    vec2 o4 = o3.yw * d.x + o3.xz * (1.0 - d.x);

    return o4.y * d.y + o4.x * (1.0 - d.y);
}

//// 3d SIMPLEX NOISE /////
vec3 mod289(vec3 x) {
    return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec4 permute(vec4 x) {
    return mod289(((x*34.0)+1.0)*x);
}

vec4 taylorInvSqrt(vec4 r)
{
    return 1.79284291400159 - 0.85373472095314 * r;
}

const mat2 m2 = mat2( 0.60, -0.80, 0.80, 0.60 );

const mat3 m3 = mat3( 0.00,  0.80,  0.60,
                     -0.80,  0.36, -0.48,
                     -0.60, -0.48,  0.64 );

float fbm( in vec3 p ) {
    float f = 0.0;
    f += 0.5000*noise( p ); p = m3*p*2.02;
    f += 0.2500*noise( p ); p = m3*p*2.03;
    f += 0.1250*noise( p ); p = m3*p*2.01;
    f += 0.0625*noise( p );
    return f/0.9375;
}

float cubeSignal(float time) { 
    float bitDepth = 8.0 * (7.5 + 4.5*cos(cos(cos(0.1*time))));
    float signal = fbm(vec3(0.3*time)); // 0 <= abs(signal) <= 1.0
    float scale = 15.0;
    return scale * (1.0 / bitDepth) * floor(bitDepth*signal);
}

// http://iquilezles.org/www/articles/smin/smin.htm
float smin( float a, float b, float k )
{
    float h = max(k-abs(a-b),0.0);
    return min(a, b) - h*h*0.25/k;
}

// http://iquilezles.org/www/articles/smin/smin.htm
float smax( float a, float b, float k )
{
    float h = max(k-abs(a-b),0.0);
    return max(a, b) + h*h*0.25/k;
}

float sdRoundBox( vec3 p, vec3 b, float r )
{
    vec3 q = abs(p) - b;
    return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0) - r;
}

float sph( ivec3 i, vec3 f, ivec3 c )
{
   // random radius at grid vertex i+c
//    float rad = 0.5*noise(i+c);
//    vec3  p = 17.0*fract( vec3(i+c)*0.3183099+vec3(0.11,0.17,0.13) );
//    float w = fract( p.x*p.y*p.z*(p.x+p.y+p.z) );
//    float r = 0.1*w*w;
   float r = 0.5*noise(vec3(i+c));
   // distance to sphere at grid vertex i+c
   return length(f-vec3(c)) - r; 
}

float box (ivec3 i, vec3 f, ivec3 c) { 
    // float r = 0.1*noise(vec3(i+c));
    vec3  p = 17.0*fract( vec3(i+c)*0.3183099+vec3(0.11,0.17,0.13) );
   float w = fract( p.x*p.y*p.z*(p.x+p.y+p.z) );
   float r = 0.7*w*w;
    return sdRoundBox( f-vec3(c), vec3(r), r*0.01 );
}

float sdBase( vec3 p )
{
   ivec3 i = ivec3(floor(p));
    vec3 f =       fract(p);
   // distance to the 8 corners spheres
   return min(min(min(box(i,f,ivec3(0,0,0)),
                      box(i,f,ivec3(0,0,1))),
                  min(box(i,f,ivec3(0,1,0)),
                      box(i,f,ivec3(0,1,1)))),
              min(min(box(i,f,ivec3(1,0,0)),
                      box(i,f,ivec3(1,0,1))),
                  min(box(i,f,ivec3(1,1,0)),
                      box(i,f,ivec3(1,1,1)))));
}

float sdFbm( vec3 p, float d )
{
   float s = 1.0;
   float t = 0.0;
   for( int i=0; i<11; i++ )
   {
       // evaluate new octave
       float n = s*sdBase(p);
	
       // add
       n = smax(n,d-0.1*s,0.3*s);
       d = smin(n,d      ,0.3*s);

       t += d; 
	
       // prepare next octave
       p = mat3( 0.00, 1.60, 1.20,
                -1.60, 0.72,-0.96,
                -1.20,-0.96, 1.28 )*p;

       p.z += 3.23*t*s; // magic numbers, level of distortion
       s = 0.422*s;
   }
   return d;
}

vec2 map (vec3 p, float time) 
{ 
    vec2 res = vec2(1e10, 0.0);

    // ground
    // float d = length(p-vec3(0.0,-100.0,0.0))-100.0;
    
    // terrain
    // float dt = sdFbm( p, d );

    // res = vec2(dt, 15.0);
    // rotate
    // p = p + vec3(0.0, 0.0, 15.0);
    
    vec3 rotPos = p + vec3(0.0, 0.0, 15.0);
    float theta = cubeSignal(time);
    vec3 n = normalize(vec3(0.11, 1.0, 0.33));
    vec4 q = vec4(cos(theta / 2.0), sin(theta / 2.0) * n);
    vec3 temp = cross(q.xyz, rotPos) + q.w * rotPos;
    vec3 rotated = rotPos + 2.0*cross(q.xyz, temp);

    res = vec2(sdRoundBox(rotated, vec3(0.5), 0.0), 15.0);

    return res;
}

vec2 raycast (in vec3 ro, in vec3 rd, float time)
{
    vec2 res = vec2(-1.0,-1.0);

    float tmin = 0.001;
    float tmax = 100.0;

    float eps = 0.0015;
    float t = tmin;
    for( int i = 0; i < 228 && t < tmax; i++) {
        vec2 h = map( ro + rd*t, time );

        if( abs(h.x) < eps){
            res = vec2(t, h.y);
            break;
        } 

        t += h.x;
    }

    return res;
}

vec3 calcNormal( in vec3 p, float time )
{
    const float eps = 0.0001; 
    const vec2 h = vec2(eps,0);
    return normalize( vec3(map(p+h.xyy, time).x - map(p-h.xyy, time).x,
                        map(p+h.yxy, time).x - map(p-h.yxy, time).x,
                        map(p+h.yyx, time).x - map(p-h.yyx, time).x ) );
}

vec2 csqr( vec2 a )  { return vec2( sin(a.x)*cos(a.y), 1.5*a.x*a.y  ); }

float mapMarble( vec3 p, float time ) { 
	float res = 0.;

    p *= 0.2;
    vec3 c = p;
    c.y *= 2.5;
	for (int i = 0; i < 12; ++i) {
        p =.82*abs(p)/dot(p,p)-0.82;
        p.yz= csqr(p.yz);
        p=p.zxy;
        res += exp(-25. * abs(dot(p,c)));
        
	}
	return res/2.;
}

vec3 marchMarbleColor(vec3 ro, vec3 rd, float tmin, float tmax, float time) { 
    float t = tmin;
    float dt = .1;
    vec3 col= vec3(0.);
    float c = 0.;
    for( int i=0; i<50; i++ )
	{
        t+=dt*exp(-2.*c);
        if(t>tmax)break;
        
        c = mapMarble(ro+t*rd, time);               

        col = .9*col+ .1*vec3(c*c);
    }    
    return col;
}

vec3 render(in vec3 ro, in vec3 rd, float time) 
{ 
    vec3 col = vec3(0.95);

    vec2 res = raycast(ro,rd, time);
    float t = res.x;
    float m = res.y;

    vec3 pos = ro + rd*t;

    // lighting
    if ( m > 5.0 ) { 
        vec3 lig = vec3(0.4, 0.8, 0.2);
        vec3 nor = calcNormal(pos, time);
        col = vec3(0.8,0.6,0.1)*dot(lig, nor);
        // col = vec3(0.1);
    }
    
    return vec3( clamp(col, 0.0, 1.0) );
}

mat3 setCamera( in vec3 ro, in vec3 ta, float cr )
{
    vec3 cw = normalize(ta-ro);
    vec3 cp = vec3(sin(cr), cos(cr),0.0);
    vec3 cu = normalize( cross(cw,cp) );
    vec3 cv =          ( cross(cu,cw) );
    return mat3( cu, cv, cw );
}

void main() {
    // camera
    vec3 ro = vec3( 0.0, 0.0, 1.0);
    vec3 ta = vec3( 0.0, 0.0, 0.0);

    mat3 ca = setCamera(ro, ta, 0.0);
    float aspect = uResolution.x / uResolution.y;
    
    vec2 p = vec2(aspect, 1.0) * (vUv - vec2(0.5));
    float time = uTime;

    // ray direction
    vec3 rd = ca * normalize( vec3(p, 5.0) );

    vec3 col = render( ro, rd, time );

    // vec3 col = vec3(1.0,0,0);
    // vec3 col = marchMarbleColor(ro * 10.0, rd * 1.25, 0.01, 1.0, time);
    // col = pow(col, vec3(0.4545));

    // "gold foil" feeling textures 
    // vec3 p3 = 8.0 * vec3(p, 0.0);

    // pure color + smudges/indents (check out that slide from inigo quelez?) - should look more like it 
    // vec3 p3 = 20.0 * rd + vec3(0.0, 0.0, 0.01*time);
    // vec3 col = vec3(0.84, .457, 0.046);
    // vec3 col2 = vec3( 0.99, .62, .01 );
    // float isoline = 0.525;
    // col = 1.0*(0.4*col + 0.6*col2*clamp(fbm(p3 + fbm(10.0*p3)) + isoline, 0., 1.)) + 0.5*(fbm(0.1*p3*fbm(p3)) - 0.75);

    // animated, cool, not really it
    // vec3 p3 = 30.0 * rd + vec3(0.0, time * 0.01, time * 0.1);
    // vec3 col = 2.0 * vec3( 0.74, .607, .086 ) * fbm( p3 + fbm( p3 + fbm( p3 ) ) ) * vec3( 0.99, .78, .01 ) * fbm( p3 + fbm( p3 ) );
    // vec3 col = vec3( 0.74, .607, .086 ) * fbm( p3 + fbm( p3 + fbm( p3 ) ) ) + 0.26 * vec3( 0.99, .78, .01 ) * fbm( p3 + fbm( p3 ) );
    
    col = pow(col, vec3(0.4545));

    o_FragColor = vec4( col, 1.0 );
}