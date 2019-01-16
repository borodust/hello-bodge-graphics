#version 330

in vec2 position;
in vec4 color;

out vec4 v_color;

void main () {
  // No transofmations, just put the vertices on Z plane as is
  gl_Position = vec4(position, 0.0, 1.0);
  // And pass color data to fragment shader:
  // it will be interpolated for us automatically
  v_color = color;
}
