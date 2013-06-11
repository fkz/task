module WebApp where


data App = {
  _heist :: Snaplet Heist,
  _database :: Snaplet Database
  }