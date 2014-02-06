

class Cipher key msg ct where
  encrypt :: key -> msg -> ct
  decrypt :: key -> ct -> msg