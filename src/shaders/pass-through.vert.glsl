#version 330

in vec2 v_Position;

void main () {
  gl_Position = vec4(v_Position, 0.0, 1.0);
  gl_PointSize = 30.0f;
}
