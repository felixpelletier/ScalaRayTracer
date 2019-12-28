case class Scene(solids : List[SolidObject], lights : List[Light]) {
  def cameraPosition = Vector3D(0, 0, 0)
}
