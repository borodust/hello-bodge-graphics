#version 330

void main () {
  // No transofmations, just put all vertices in the middle of the screen
  gl_Position = vec4(0.0, 0.0, 0.0, 1.0);
  // Enlarge drawn points so we can easily recognize the result -
  // not required but handy for debugging shaders
  gl_PointSize = 100.0f;
}
