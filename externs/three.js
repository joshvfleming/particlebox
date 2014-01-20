var THREE = {
  SphereGeometry: function(){},
  MeshNormalMaterial: function(){},
  Mesh: function(){},
  PerspectiveCamera: function(){},
  Scene: function(){},
  CanvasRenderer: function(){}
};

THREE.Mesh.prototype = {
  position: {
    x: 0, y: 0, z: 0
  },

  rotation: {
    x: 0, y: 0, z: 0
  }
};

THREE.PerspectiveCamera.prototype = {
  position: {
    x: 0, y: 0, z: 0
  }
};

THREE.Scene.prototype = {
  add: function(){}
};

THREE.CanvasRenderer.prototype = {
  setSize: function(){},
  render: function(){},
  domElement: null
};
