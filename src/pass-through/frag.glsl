#version 330

out vec4 fragColor;

void main () {
  // hardcode output color for any fragments coming out from our pipeline
  fragColor = vec4(0.8, 0.8, 0.8, 1.0);
}
