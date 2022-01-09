#version 300 es
#define AA 3

precision highp float;

in vec2 vUv;
uniform vec2 uResolution;
uniform float uTime;

out vec4 o_FragColor;

float cubeSignal(float time) { 
    return cos(time*0.35);
}

/*
 
                                                    
                                                    
 `7MN.   `7MF' .g8""8q. `7MMF' .M"""bgd `7MM"""YMM  
   MMN.    M .dP'    `YM. MM  ,MI    "Y   MM    `7  
   M YMb   M dM'      `MM MM  `MMb.       MM   d    
   M  `MN. M MM        MM MM    `YMMNq.   MMmmMM    
   M   `MM.M MM.      ,MP MM  .     `MM   MM   Y  , 
   M     YMM `Mb.    ,dP' MM  Mb     dM   MM     ,M 
 .JML.    YM   `"bmmd"' .JMML.P"Ybmmd"  .JMMmmmmMMM 
                                                    
                                                    
 
*/
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

float snoise(vec3 v)
{ 
    const vec2  C = vec2(1.0/6.0, 1.0/3.0) ;
    const vec4  D = vec4(0.0, 0.5, 1.0, 2.0);

    // First corner
    vec3 i  = floor(v + dot(v, C.yyy) );
    vec3 x0 =   v - i + dot(i, C.xxx) ;

    // Other corners
    vec3 g = step(x0.yzx, x0.xyz);
    vec3 l = 1.0 - g;
    vec3 i1 = min( g.xyz, l.zxy );
    vec3 i2 = max( g.xyz, l.zxy );

    //   x0 = x0 - 0.0 + 0.0 * C.xxx;
    //   x1 = x0 - i1  + 1.0 * C.xxx;
    //   x2 = x0 - i2  + 2.0 * C.xxx;
    //   x3 = x0 - 1.0 + 3.0 * C.xxx;
    vec3 x1 = x0 - i1 + C.xxx;
    vec3 x2 = x0 - i2 + C.yyy; // 2.0*C.x = 1/3 = C.y
    vec3 x3 = x0 - D.yyy;      // -1.0+3.0*C.x = -0.5 = -D.y

    // Permutations
    i = mod289(i); 
    vec4 p = permute( permute( permute( 
                i.z + vec4(0.0, i1.z, i2.z, 1.0 ))
            + i.y + vec4(0.0, i1.y, i2.y, 1.0 )) 
            + i.x + vec4(0.0, i1.x, i2.x, 1.0 ));

    // Gradients: 7x7 points over a square, mapped onto an octahedron.
    // The ring size 17*17 = 289 is close to a multiple of 49 (49*6 = 294)
    float n_ = 0.142857142857; // 1.0/7.0
    vec3  ns = n_ * D.wyz - D.xzx;

    vec4 j = p - 49.0 * floor(p * ns.z * ns.z);  //  mod(p,7*7)

    vec4 x_ = floor(j * ns.z);
    vec4 y_ = floor(j - 7.0 * x_ );    // mod(j,N)

    vec4 x = x_ *ns.x + ns.yyyy;
    vec4 y = y_ *ns.x + ns.yyyy;
    vec4 h = 1.0 - abs(x) - abs(y);

    vec4 b0 = vec4( x.xy, y.xy );
    vec4 b1 = vec4( x.zw, y.zw );

    //vec4 s0 = vec4(lessThan(b0,0.0))*2.0 - 1.0;
    //vec4 s1 = vec4(lessThan(b1,0.0))*2.0 - 1.0;
    vec4 s0 = floor(b0)*2.0 + 1.0;
    vec4 s1 = floor(b1)*2.0 + 1.0;
    vec4 sh = -step(h, vec4(0.0));

    vec4 a0 = b0.xzyw + s0.xzyw*sh.xxyy ;
    vec4 a1 = b1.xzyw + s1.xzyw*sh.zzww ;

    vec3 p0 = vec3(a0.xy,h.x);
    vec3 p1 = vec3(a0.zw,h.y);
    vec3 p2 = vec3(a1.xy,h.z);
    vec3 p3 = vec3(a1.zw,h.w);

    //Normalise gradients
    vec4 norm = taylorInvSqrt(vec4(dot(p0,p0), dot(p1,p1), dot(p2, p2), dot(p3,p3)));
    p0 *= norm.x;
    p1 *= norm.y;
    p2 *= norm.z;
    p3 *= norm.w;

    // Mix final noise value
    vec4 m = max(0.5 - vec4(dot(x0,x0), dot(x1,x1), dot(x2,x2), dot(x3,x3)), 0.0);
    m = m * m;
    return 105.0 * dot( m*m, vec4( dot(p0,x0), dot(p1,x1), 
                                    dot(p2,x2), dot(p3,x3) ) );
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

vec3 hash3( vec2 p ){
    vec3 q = vec3( dot(p,vec2(127.1,311.7)), 
				   dot(p,vec2(269.5,183.3)), 
				   dot(p,vec2(419.2,371.9)) );
	return fract(sin(q)*43758.5453);
}

float fbm( in vec2 x, float u, float v )
{
    vec2 p = floor(x);
    vec2 f = fract(x);

    float k = 1.0 + 63.0*pow(1.0-v,4.0);
    float va = 0.0;
    float wt = 0.0;
    for( int j=-2; j<=2; j++ )
    for( int i=-2; i<=2; i++ )
    {
        vec2  g = vec2( float(i), float(j) );
        vec3  o = hash3( p + g )*vec3(u,u,1.0);
        vec2  r = g - f + o.xy;
        float d = dot(r,r);
        float w = pow( 1.0-smoothstep(0.0,1.414,sqrt(d)), k );
        va += w*o.z;
        wt += w;
    }

    return va/wt;
}

float fbmOctaves (in vec2 x, float u, float v ) {
    float t = -.5;

    for (float f = 1.0 ; f <= 10.0 ; f++ ){
        float power = pow( 2.0, f );
        t += abs( fbm( power * x, u, v ) / power );
    }

    return t;
}

/*
 
                                       
                                       
  .M"""bgd `7MM"""Yb. `7MM"""YMM       
 ,MI    "Y   MM    `Yb. MM    `7       
 `MMb.       MM     `Mb MM   d ,pP"Ybd 
   `YMMNq.   MM      MM MM""MM 8I   `" 
 .     `MM   MM     ,MP MM   Y `YMMMa. 
 Mb     dM   MM    ,dP' MM     L.   I8 
 P"Ybmmd"  .JMMmmmdP' .JMML.   M9mmmP' 
                                       
                                       
 
*/
float sdfSphere(vec3 p, float r) {
    return length( p ) - r;
}

float sdTorus( vec3 p, vec2 t )
{
    vec2 q = vec2(length(p.xz)-t.x,p.y);
    return length(q)-t.y;
}

float sdBox( vec3 p, vec3 b )
{
  vec3 q = abs(p) - b;
  return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0);
}

float sdRoundBox( vec3 p, vec3 b, float r )
{
    vec3 q = abs(p) - b;
    return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0) - r;
}

float sdbEllipsoid( in vec3 p, in vec3 r )
{
    float k1 = length(p/r);
    float k2 = length(p/(r*r));
    return k1*(k1-1.0)/k2;
}

vec2 sdCapsule( vec3 p, vec3 a, vec3 b )
{
  vec3 pa = p - a, ba = b - a;
  float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
  return vec2( length( pa - ba*h ) - 0.01, h );
}


/*
 
                                                                                      
                                                                                      
 `7MMM.     ,MMF'      db      `7MM"""Mq.`7MM"""Mq.`7MMF'`7MN.   `7MF' .g8"""bgd      
   MMMb    dPMM       ;MM:       MM   `MM. MM   `MM. MM    MMN.    M .dP'     `M      
   M YM   ,M MM      ,V^MM.      MM   ,M9  MM   ,M9  MM    M YMb   M dM'       `      
   M  Mb  M' MM     ,M  `MM      MMmmdM9   MMmmdM9   MM    M  `MN. M MM               
   M  YM.P'  MM     AbmmmqMA     MM        MM        MM    M   `MM.M MM.    `7MMF'    
   M  `YM'   MM    A'     VML    MM        MM        MM    M     YMM `Mb.     MM      
 .JML. `'  .JMML..AMA.   .AMMA..JMML.    .JMML.    .JMML..JML.    YM   `"bmmmdPY      
                                                                                      
                                                                                      
 
*/

vec2 map (vec3 p, float time) { 
    vec2 res = vec2(1e10, 0.0);

    // rotate
    vec3 rotPos = p + vec3(-0.5, -3.5, 15.0);
    float theta = time+55.0*cubeSignal(time);
    vec3 n = normalize(vec3(1.0, 1.0, 0.0));
    vec4 q = vec4(cos(theta / 2.0), sin(theta / 2.0) * n);
    vec3 temp = cross(q.xyz, rotPos) + q.w * rotPos;
    vec3 rotated = rotPos + 2.0*cross(q.xyz, temp);
    
    // twist
    float k = (sin(time*0.35))*2.75; 
    float c = cos(k*rotated.y);
    float s = sin(k*rotated.y);
    mat2  m = mat2(c,-s,s,c);
    vec3 twisted = vec3(m*rotated.xz,rotated.y);

    float cubeAmount = 0.5+0.5*cubeSignal(time);
    float invCube = 1.0 - cubeAmount;
    res = vec2(sdRoundBox(twisted, vec3(0.5 * invCube), 0.01 + 0.99*cubeAmount), 15.0);
    res.x *= 0.8;
    return res;
}

vec2 raycast (in vec3 ro, in vec3 rd, float time){
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

/*
 
                                                                                             
                                                                                             
 `7MMF'   `7MF'MMP""MM""YMM `7MMF'`7MMF'      `7MMF'MMP""MM""YMM `7MMF'`7MM"""YMM   .M"""bgd 
   MM       M  P'   MM   `7   MM    MM          MM  P'   MM   `7   MM    MM    `7  ,MI    "Y 
   MM       M       MM        MM    MM          MM       MM        MM    MM   d    `MMb.     
   MM       M       MM        MM    MM          MM       MM        MM    MMmmMM      `YMMNq. 
   MM       M       MM        MM    MM      ,   MM       MM        MM    MM   Y  , .     `MM 
   YM.     ,M       MM        MM    MM     ,M   MM       MM        MM    MM     ,M Mb     dM 
    `bmmmmd"'     .JMML.    .JMML..JMMmmmmMMM .JMML.   .JMML.    .JMML..JMMmmmmMMM P"Ybmmd"  
                                                                                             
                                                                                             
 
*/
vec3 palette( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d )
{  return a + b*cos( 6.28318*(c*t+d) ); }


vec3 rotatePoint(vec3 p, vec3 n, float theta) { 
    vec4 q = vec4(cos(theta / 2.0), sin(theta / 2.0) * n);
    vec3 temp = cross(q.xyz, p) + q.w * p;
    vec3 rotated = p + 2.0*cross(q.xyz, temp);
    return rotated;
}

vec3 calcNormal( in vec3 p, float time )
{
    const float eps = 0.0001; 
    const vec2 h = vec2(eps,0);
    return normalize( vec3(map(p+h.xyy, time).x - map(p-h.xyy, time).x,
                        map(p+h.yxy, time).x - map(p-h.yxy, time).x,
                        map(p+h.yyx, time).x - map(p-h.yyx, time).x ) );
}

float calcAO( in vec3 pos, in vec3 nor, float time )
{
	float occ = 0.0;
    float sca = 1.0;
    for( int i=0; i<5; i++ )
    {
        float h = 0.01 + 0.12*float(i)/4.0;
        float d = map( pos + h*nor, time ).x;
        occ += (h-d)*sca;
        sca *= 0.95;
        if( occ>0.35 ) break;
    }
    return clamp( 1.0 - 3.0*occ, 0.0, 1.0 ) * (0.5+0.5*nor.y);
}

float calcSoftshadow( in vec3 ro, in vec3 rd, float mint, float maxt, float k, float time )
{
    float res = 1.0;
    for( float t=mint; t<maxt; )
    {
        float h = map(ro + rd*t, time).x;
        if( h< mint )
            return 0.0;
        res = min( res, k*h/t );
        t += h;
    }
    return res;
}

/*
 
                                                                                                            
                                                                                                            
 `7MM"""Mq.  `7MM"""YMM  `7MN.   `7MF'`7MM"""Yb. `7MM"""YMM  `7MM"""Mq.  `7MMF'`7MN.   `7MF' .g8"""bgd      
   MM   `MM.   MM    `7    MMN.    M    MM    `Yb. MM    `7    MM   `MM.   MM    MMN.    M .dP'     `M      
   MM   ,M9    MM   d      M YMb   M    MM     `Mb MM   d      MM   ,M9    MM    M YMb   M dM'       `      
   MMmmdM9     MMmmMM      M  `MN. M    MM      MM MMmmMM      MMmmdM9     MM    M  `MN. M MM               
   MM  YM.     MM   Y  ,   M   `MM.M    MM     ,MP MM   Y  ,   MM  YM.     MM    M   `MM.M MM.    `7MMF'    
   MM   `Mb.   MM     ,M   M     YMM    MM    ,dP' MM     ,M   MM   `Mb.   MM    M     YMM `Mb.     MM      
 .JMML. .JMM..JMMmmmmMMM .JML.    YM  .JMMmmmdP' .JMMmmmmMMM .JMML. .JMM..JMML..JML.    YM   `"bmmmdPY      
                                                                                                            
                                                                                                            
    lighting, raymarching. 
*/

vec3 render(in vec3 ro, in vec3 rd, in vec3 rdx, in vec3 rdy, float time) { 
    vec3 col = vec3(0.0);
    vec3 sunLig = normalize(vec3(0.0, 2.0, 1.0));

    col = max(vec3(1.0), vec3(0.4,0.5,0.56) * 1.925);

    vec2 res = raycast(ro,rd, time);
    float t = res.x;
    float m = res.y;

    vec3 pos = ro + rd*t;

    // MATERIALS
    vec3 material = vec3(0);
    vec2 K = vec2(0,1); // amount, power

    // lighting
    if ( m > 5.0 ) { 
        K = vec2(0.5, 10.0);

        vec3 nor = calcNormal(pos, time);
        material = 2.0*palette(time*0.1 + fbm(pos+fbm(3.0* (0.15*nor + 0.25*pos)) ), 
                            vec3(0.5), 
                            vec3(0.5),
                            vec3(2.0, 1.0, 0.0),
                            vec3(0.8, 0.9, 0.3));

        float cubeAmount = 0.5+0.5*cubeSignal(time);
        material = cubeAmount * material + (1.0 - cubeAmount) * vec3(0.5);

        float occ = calcAO(pos, nor, time);
        vec3 hal  = normalize(rd - sunLig);
        float fre = clamp(1.0 + dot(nor, rd), 0.0, 1.0);
        float shadow = 1.5*calcSoftshadow(pos, sunLig, 0.1, 10.0, 6.0, time);
        
        float bou = clamp( 0.3-0.7*nor.y, 0.0, 1.0 );

        vec3 ref = reflect( rd, nor );
        vec3 lin = vec3(0);

        // sun
        {
            float dif = clamp(dot(nor, hal), 0.0, 1.0);
            dif *= shadow;
            float spe = K.x * pow( clamp(dot(nor, hal), 0.0, 1.0), K.y );
            spe *= dif;
            spe *= 0.04+0.96*pow(clamp(1.0-dot(hal,sunLig),0.0,1.0),5.0); // fresnel
            lin += material * (3.20) * dif * vec3(2.1,1.1,0.6);
            lin += material * (12.) * spe * vec3(2.1,1.1,0.6);
        }
        // sky / reflections
        {
            float dif = occ * sqrt(clamp(0.2 + 0.8 * nor.y, 0.0, 1.0));
            vec3 skyColor = vec3(0.3,0.4,0.56);
            lin += material * 0.6 * dif * (0.55 * clamp(skyColor, 0.0, 0.6));

            float spe = smoothstep( -0.2, 0.2, ref.y );
            spe *= dif;
            spe *= occ * occ * K.x * 0.04 + 0.96 * pow( fre, 5.0 );
            spe *= shadow; 
            lin += 3.0 * spe * skyColor;
        }
        // ground / bounce light
        {
            lin += 3.0 * material * vec3(0.5,0.41,0.39) * bou * occ;
        }
        // back
        {
        	float dif = clamp( dot( nor, normalize(vec3(-0.5,0.0,-0.75))), 0.0, 1.0 )*clamp( 1.0-pos.y,0.0,1.0);
                  dif *= occ;
        	lin += 3.0*material*0.55*dif*vec3(0.35,0.25,0.35);
        }
        // sss
        {
            float dif = clamp(dot(nor, hal), 0.0, 1.0);
            lin += 5.0 * fre * fre * (0.2 + 0.8 * dif * occ * shadow) * material;
        }

        col = (0.04 * material) + (0.96 * lin);
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
    vec3 ro = vec3( 0.0, 3.5, 1.0);
    vec3 ta = vec3( 0.0, 3.5, 0.0);

    mat3 ca = setCamera(ro, ta, 0.0);

    float aspect = uResolution.x / uResolution.y;

    vec3 total = vec3(0.0);
#if AA>1
    for (int m=0; m < AA; m++)
    for (int n=0; n < AA; n++) { 
        vec2 o = (vec2(float(m), float(n)) / uResolution) / float(AA);
        vec2 p = vec2(aspect, 1.0) * ( (vUv+o) - vec2(0.5));
        float time = uTime + 2.0 * (1.0/48.0) * float(m * n * AA) / float(AA*AA);
#else
        vec2 p = vec2(aspect, 1.0) * (vUv - vec2(0.5));
        float time = uTime;
#endif
        time *= 0.9;
        vec2 pInv = vec2(1.0) - p;
        // ray direction
        vec3 rd = ca * normalize( vec3(p, 5.2) );

        // the more fbm, the more rd points to the cube)
        float amount = 0.5 + 0.5*cubeSignal(time);
        // this ver using 'p' -- slight modification to multiply 0.125 * p outside
        rd.xy += amount * 0.125 * vec2(1.0,0.0) * fbm( (amount*15.5 + 3.0) * vec3(fbm(6.0 * vec3(0.5+0.5*snoise(6.0 * amount * vec3(pInv,1.0) + 0.1*time)*p, 0.0))*p*1.0, 0.0 ) );
        rd.xy += amount * 0.0625 * vec2(3.0,0.0) * fbm( (amount*30.5 + 3.0) * vec3(fbm(2.0 * vec3(0.5+0.5*snoise(6.0 * amount * vec3(p,0.0) + 0.1*time)*p, 0.0))*vec2(0.4, 0.4), 0.0 ) );
        // rd = normalize(rd);

        // ray differentials 
        vec2 px =  vec2(aspect, 1.0) * ( (vUv+vec2(1.0,0.0)) - vec2(0.5));
        vec2 py =  vec2(aspect, 1.0) * ( (vUv+vec2(0.0,1.0)) - vec2(0.5));
        vec3 rdx = ca * normalize( vec3(px, 2.5));
        vec3 rdy = ca * normalize( vec3(py, 2.5));

        vec3 color = render( ro, rd, rdx, rdy, time );

        color = pow(color, vec3(0.5245));

        total += color;
#if AA>1
    }
    total /= float(AA*AA);
#endif
    
    // color correction / post processing
    total = min(total, 1.0);

    o_FragColor = vec4( total, 1.0 );
}