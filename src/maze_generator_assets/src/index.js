import { maze_generator } from "../../declarations/maze_generator";

document.getElementById("generateBtn").addEventListener("click", async () => {
  const size = BigInt(document.getElementById("size").value);
  const maze = await maze_generator.generate(size);

  document.getElementById("maze").innerText = maze;
});
