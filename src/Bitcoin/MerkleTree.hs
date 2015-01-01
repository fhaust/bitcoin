
module Bitcoin.MerkleTree (
  mtHash, 
  mtLeft, mtRight,
  merkleTree
) where

import Data.Tree
import Bitcoin.Crypto

import qualified Data.ByteString as B

type MerkleTree = Tree Hash

-- | get the hash from a node
mtHash :: MerkleTree -> Hash
mtHash = rootLabel

-- | get the left child of a node
mtLeft :: MerkleTree -> MerkleTree
mtLeft t = subForest t !! 0

-- | get the right child of a node
mtRight :: MerkleTree -> MerkleTree
mtRight t = subForest t !! 1


-- | test with official data from here:
-- | http://blockexplorer.com/block/000000000000011278004e50a6cf8f2c8abbaf1fa7d030cc3e8573ac3476efe1
--
-- small function to convert from blockexplorer strings to hashes
-- >>> let getHash = Hash . B.reverse . fst . B16.decode . B.fromString
--
-- hashes from block 230006
-- >>> let a = getHash "4a4e3dbdac02401e355a76b2268e5ed9251c4386a5a0de5cf34fdfb2aef68b80"
-- >>> let b = getHash "07583643e9368492602051dbe52068802c7039c64c6defeeedd1f8a39c7737bc"
-- >>> let c = getHash "303a3bfa5007cedbaf4e5acd7e8a591c7f405b0f4834675617ded260239a1698"
-- >>> let d = getHash "5443fbb50ecd9c92713d8dec0700da85fa70a62018df9b56f86ccdd61293f2ae"
-- >>> let e = getHash "3297b0a7f724484d4cdc05446aea1c9de17f5c0ee667dc299a631b764fb35f34"
-- >>> let f = getHash "3c80aa36716cbc21686cbb252e474d49433330afa8ff14b11c17e5a59ef3411e"
--
-- calculate merkle tree and get root node
-- >>> let t = mtHash $ merkleTree [a,b,c,d,e,f]
-- 
-- merkle root from block 230006
-- >>> let r = getHash "3d55e5375ff7c223c2c3140fe3b1bf3701c53dbfb14154c302eeb716f872dd2b"
--
-- compare them
-- >>> t == r
-- True


merkleTree :: [Hash] -> MerkleTree
merkleTree hs = head . merkleTree' $ hs'
  where hs' = map (`Node` []) hs


merkleTree' :: [MerkleTree] -> [MerkleTree]
merkleTree' hs | null $ tail hs = hs
               | otherwise      = merkleTree' $ go hs
  where go (a:b:xs) = subTree a b : go xs
        go (a:[])   = [subTree a a] 
        go []       = [] 

        subTree a b = 
          Node (hashConcat (mtHash a) (mtHash b)) [a, b]



-- | combine two hashes by concatinating them and hash the resulting string
hashConcat :: Hash -> Hash -> Hash
hashConcat a b = btcHash $ B.append (unHash a) (unHash b)
