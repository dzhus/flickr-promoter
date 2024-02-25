module Promoter.Rules where

import ClassyPrelude hiding (any)
import GHC.Records
import Lens.Micro
import Promoter.Types
import Data.List (nub)

newtype Rule = Rule (Photo -> Bool, GroupId)

(.=>) :: (Photo -> Bool) -> GroupId -> Rule
(.=>) isSource target = Rule (isSource, target)

any :: Photo -> Bool
any = const True

-- | >>> locatedIn "UK"
-- False
locatedIn :: Text -> Photo -> Bool
locatedIn text photo =
  case getField @"location" photo of
    Just loc -> isInfixOf text $ unLocation loc
    Nothing -> False

hasTag :: Tag -> Photo -> Bool
hasTag tag p = tag `elem` (p & getField @"tags")

moreFavesThan :: Word -> (Photo -> Bool)
moreFavesThan threshold photo = faves photo >= threshold

-- TODO add more England groups
rules :: [Rule]
rules =
  [ moreFavesThan 1 .=> "1755214@N23", -- https://www.flickr.com/groups/the-best-of-flickr/
    moreFavesThan 1 .=> "1136489@N22", -- https://www.flickr.com/groups/1136489@N22/
    moreFavesThan 1 .=> "1902869@N24", -- https://www.flickr.com/groups/fr_unofficial/
    moreFavesThan 1 .=> "34427469792@N01", -- https://www.flickr.com/groups/central/
    moreFavesThan 1 .=> "3537491@N25", -- https://www.flickr.com/groups/3537491@N25/
    -- moreFavesThan 1 .=> "3873469@N24", -- https://www.flickr.com/groups/artiste24/
    moreFavesThan 1 .=> "40732569271@N01", -- https://www.flickr.com/groups/photography-group/
    moreFavesThan 1 .=> "416556@N22",
    moreFavesThan 1 .=> "76535076@N00", -- https://www.flickr.com/groups/flickraddicts/
    moreFavesThan 1 .=> "11252682@N00", -- https://www.flickr.com/groups/the_world_through_my_eyes/
    moreFavesThan 1 .=> "28747776@N00",
    moreFavesThan 1 .=> "91514935@N00",
    moreFavesThan 1 .=> "52240402017@N01",
    moreFavesThan 1 .=> "14805334@N23",
    moreFavesThan 1 .=> "68567710@N00", -- https://www.flickr.com/groups/postcard/
    moreFavesThan 1 .=> "58286952@N00",
    moreFavesThan 1 .=> "20759249@N00",
    moreFavesThan 1 .=> "2161940@N25", -- https://flickr.com/groups/brilliantflickr/
    any .=> "557255@N22",
    any .=> "95309787@N00",
    any .=> "1148171@N20",
    any .=> "58898522@N00", -- https://www.flickr.com/groups/art-photography/
    any .=> "38436807@N00", -- https://www.flickr.com/groups/flickrtoday/
    any .=> "2677807@N23", -- https://www.flickr.com/groups/amateurphotographer/
    any .=> "43501458@N00", -- https://www.flickr.com/groups/amateurs/
    any .=> "769299@N22", -- https://www.flickr.com/groups/769299@N22/
    any .=> "2978869@N23", -- https://www.flickr.com/groups/2978869@N23/
    locatedIn "Bavaria" .=> "860590@N23",
    locatedIn "Crimea" .=> "60453939@N00",
    locatedIn "England" .=> "35468144964@N01",
    locatedIn "France" .=> "52241533836@N01",
    locatedIn "London" .=> "2625353@N20",
    locatedIn "Lyon" .=> "13409106@N00",
    locatedIn "Paris" .=> "36101698174@N01",
    locatedIn "Prague" .=> "48889111127@N01",
    locatedIn "Italy" .=> "31746602@N00",
    locatedIn "Italy" .=> "37996580003@N01",
    locatedIn "Rome" .=> "59943000@N00",
    locatedIn "Russia" .=> "288127@N25", -- https://www.flickr.com/groups/ru/
    locatedIn "Scotland" .=> "37887068055@N01",
    locatedIn "Switzerland" .=> "41894179852@N01",
    locatedIn "Switzerland" .=> "67376880@N00",
    hasTag "landscape" .=> "13197975@N00",
    hasTag "landscape" .=> "650323@N24", -- https://www.flickr.com/groups/ngmanimallovers/
    hasTag "landscape" .=> "2241717@N21",
    hasTag "landscape" .=> "23854677@N00", -- https://www.flickr.com/groups/landcape/
    hasTag "nature" .=> "81431815@N00", -- https://www.flickr.com/groups/natur/
    hasTag "landscape" .=> "80148101@N00", -- https://www.flickr.com/groups/natureandlandscapes/
    hasTag "landscape" .=> "11611663@N00", -- https://www.flickr.com/groups/nature-and-photography/
    hasTag "landscape" .=> "535727@N21", -- https://www.flickr.com/groups/worldlandscapes/
    hasTag "landscape" .=> "1003995@N21" -- https://www.flickr.com/groups/landscape-beauty/
    -- hasTag "landscape" .=> "78249294@N00"
  ]

-- | Which groups to post this photo to
matchingGroups :: Photo -> [GroupId]
matchingGroups photo = nub $
  catMaybes $
    flip map rules $
      \(Rule (predicate, targetGroup)) ->
        if predicate photo && targetGroup `notElem` groups photo
          then Just targetGroup
          else Nothing
