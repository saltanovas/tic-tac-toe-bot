module Task3Message
where

-- ┌       ┐
-- │ X   X │
-- │ X O O │
-- │ X   O │
-- └       ┘
-- encoding: BenMap

message1 :: String
message1 = "d4:lastd1:0d4:datad1:0i0e1:1i0e1:21:Xeee4:prevd4:lastd1:0d4:datad1:0i1e1:1i1e1:21:Oeee4:prevd4:prevd4:prevd4:lastd1:0d4:datad1:0i0e1:1i1e1:21:Xeee4:prevd4:prevd4:lastd1:0d4:datad1:0i0e1:1i2e1:21:Xeeee4:lastd1:0d4:datad1:0i2e1:1i1e1:21:Oeeeee4:lastd1:0d4:datad1:0i2e1:1i2e1:21:Oeeee4:lastd1:0d4:datad1:0i2e1:1i0e1:21:Xeeeeee"

-- Raw message bellow:
-- d4:lastd1:0d4:datad1:0i0e1:1i0e1:21:Xeee4:prevd4:lastd1:0d4:datad1:0i1e1:1i1e1:21:Oeee4:prevd4:prevd4:prevd4:lastd1:0d4:datad1:0i0e1:1i1e1:21:Xeee4:prevd4:prevd4:lastd1:0d4:datad1:0i0e1:1i2e1:21:Xeeee4:lastd1:0d4:datad1:0i2e1:1i1e1:21:Oeeeee4:lastd1:0d4:datad1:0i2e1:1i2e1:21:Oeeee4:lastd1:0d4:datad1:0i2e1:1i0e1:21:Xeeeeee



-- ┌       ┐
-- │ X X X │
-- │ O X O │
-- │ X O O │
-- └       ┘
-- encoding: BenMap

message2 :: String
message2 = "d4:prevd4:prevd4:prevd4:prevd4:lastd1:0d4:datad1:0i0e1:1i2e1:21:Xeee4:prevd4:lastd1:0d4:datad1:0i1e1:1i2e1:21:Oeee4:prevd4:lastd1:0d4:datad1:0i0e1:1i0e1:21:Xeee4:prevd4:lastd1:0d4:datad1:0i2e1:1i1e1:21:Oeee4:prevd4:lastd1:0d4:datad1:0i1e1:1i0e1:21:Xeeeeeeee4:lastd1:0d4:datad1:0i2e1:1i2e1:21:Oeeee4:lastd1:0d4:datad1:0i1e1:1i1e1:21:Xeeee4:lastd1:0d4:datad1:0i0e1:1i1e1:21:Oeeee4:lastd1:0d4:datad1:0i2e1:1i0e1:21:Xeeee"

-- Raw message bellow:
-- d4:prevd4:prevd4:prevd4:prevd4:lastd1:0d4:datad1:0i0e1:1i2e1:21:Xeee4:prevd4:lastd1:0d4:datad1:0i1e1:1i2e1:21:Oeee4:prevd4:lastd1:0d4:datad1:0i0e1:1i0e1:21:Xeee4:prevd4:lastd1:0d4:datad1:0i2e1:1i1e1:21:Oeee4:prevd4:lastd1:0d4:datad1:0i1e1:1i0e1:21:Xeeeeeeee4:lastd1:0d4:datad1:0i2e1:1i2e1:21:Oeeee4:lastd1:0d4:datad1:0i1e1:1i1e1:21:Xeeee4:lastd1:0d4:datad1:0i0e1:1i1e1:21:Oeeee4:lastd1:0d4:datad1:0i2e1:1i0e1:21:Xeeee
