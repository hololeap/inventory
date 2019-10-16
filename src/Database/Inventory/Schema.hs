{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE GADTs #-}
--{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module Database.Inventory.Schema where

import Data.Scientific

import Database.Beam
import Database.Beam.Schema
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.Beam.Postgres.Full

import Control.Monad.Identity
import Control.Monad.IO.Class (MonadIO(..))

import Data.Text (Text)
import Data.Typeable

import GHC.Generics

type Serial = SqlSerial Int

data BoxT f = Box
    { _boxId         :: Columnar f Serial
    , _boxIdentifier :: Columnar f Text
    , _boxParent     :: PrimaryKey BoxT (Nullable f)
    } deriving (Generic, Beamable)

instance Table BoxT where
    data PrimaryKey BoxT f = BoxId (Columnar f Serial)
        deriving (Generic, Beamable)
    primaryKey = BoxId . _boxId

type Box        = BoxT Identity
type BoxId      = PrimaryKey BoxT Identity
type MaybeBoxId = PrimaryKey BoxT (Nullable Identity)

deriving instance Show Box
deriving instance Show BoxId
deriving instance Show MaybeBoxId
deriving instance Eq   Box
deriving instance Eq   BoxId
deriving instance Eq   MaybeBoxId

data ItemT f = Item
    { _itemId             :: Columnar f Serial
    , _itemName           :: Columnar f Text
    , _itemBox            :: PrimaryKey BoxT f
    , _itemQuality        :: PrimaryKey QualityT (Nullable f)
    , _itemPrice          :: Columnar (Nullable f) Scientific
    , _itemNotes          :: Columnar (Nullable f) Text
    , _itemPowersOn       :: Columnar (Nullable f) Bool
    , _itemWorksCorrectly :: Columnar (Nullable f) Bool }
    deriving (Generic, Beamable)

instance Table ItemT where
    data PrimaryKey ItemT f = ItemId (Columnar f Serial)
        deriving (Generic, Beamable)
    primaryKey = ItemId . _itemId

type Item        = ItemT Identity
type ItemId      = PrimaryKey ItemT Identity
type MaybeItemId = PrimaryKey ItemT (Nullable Identity)

deriving instance Show Item
deriving instance Show ItemId
deriving instance Show MaybeItemId
deriving instance Eq   Item
deriving instance Eq   ItemId
deriving instance Eq   MaybeItemId

data QualityT f = Quality
    { _qualityId   :: Columnar f Serial
    , _qualityName :: Columnar f Text   }
    deriving (Generic, Beamable)

instance Table QualityT where
    data PrimaryKey QualityT f = QualityId (Columnar f Serial)
        deriving (Generic, Beamable)
    primaryKey = QualityId . _qualityId

type Quality        = QualityT Identity
type QualityId      = PrimaryKey QualityT Identity
type MaybeQualityId = PrimaryKey QualityT (Nullable Identity)

deriving instance Show Quality
deriving instance Show QualityId
deriving instance Show MaybeQualityId
deriving instance Eq   Quality
deriving instance Eq   QualityId
deriving instance Eq   MaybeQualityId

data TagT f = Tag
    { _tagId   :: Columnar f Serial
    , _tagName :: Columnar f Text   }
    deriving (Generic, Beamable)

instance Table TagT where
    data PrimaryKey TagT f = TagId (Columnar f Serial)
        deriving (Generic, Beamable)
    primaryKey = TagId . _tagId

type Tag   = TagT Identity
type TagId = PrimaryKey TagT Identity

deriving instance Show Tag
deriving instance Show TagId
deriving instance Eq   Tag
deriving instance Eq   TagId

data ItemTagT f = ItemTag
    { _itemtagItemId :: PrimaryKey ItemT f
    , _itemtagTagId  :: PrimaryKey TagT  f }
    deriving (Generic, Beamable)

instance Table ItemTagT where
    data PrimaryKey ItemTagT f = ItemTagIds (PrimaryKey ItemT f) (PrimaryKey TagT f)
        deriving (Generic, Beamable)
    primaryKey x = ItemTagIds (_itemtagItemId x) (_itemtagTagId x)

type ItemTag    = ItemTagT Identity
type ItemTagIds = PrimaryKey ItemTagT Identity

deriving instance Show ItemTag
deriving instance Show ItemTagIds
deriving instance Eq   ItemTag
deriving instance Eq   ItemTagIds

data GroupT f = Group
    { _groupId          :: Columnar f Serial
    , _groupDescription :: Columnar (Nullable f) Text }
    deriving (Generic, Beamable)

instance Table GroupT where
    data PrimaryKey GroupT f = GroupId (Columnar f Serial)
        deriving (Generic, Beamable)
    primaryKey = GroupId . _groupId

type Group   = GroupT Identity
type GroupId = PrimaryKey GroupT Identity

deriving instance Show Group
deriving instance Show GroupId
deriving instance Eq   Group
deriving instance Eq   GroupId

data ItemGroupT f = ItemGroup
    { _itemgroupItemId     :: PrimaryKey ItemT f
    , _itemgroupItemParent :: PrimaryKey ItemT (Nullable f)
    , _itemgroupGroupId      :: PrimaryKey GroupT f             }
    deriving (Generic, Beamable)

instance Table ItemGroupT where
    data PrimaryKey ItemGroupT f = ItemGroupIds (PrimaryKey ItemT f) (PrimaryKey GroupT f)
        deriving (Generic, Beamable)
    primaryKey x = ItemGroupIds (_itemgroupItemId x) (_itemgroupGroupId x)

type ItemGroup    = ItemGroupT Identity
type ItemGroupIds = PrimaryKey ItemGroupT Identity

deriving instance Show ItemGroup
deriving instance Show ItemGroupIds
deriving instance Eq   ItemGroup
deriving instance Eq   ItemGroupIds

data InventoryDb f = InventoryDb
    { _inventoryBox       :: f (TableEntity BoxT      )
    , _inventoryQuality   :: f (TableEntity QualityT  )
    , _inventoryItem      :: f (TableEntity ItemT     )
    , _inventoryTag       :: f (TableEntity TagT      )
    , _inventoryItemTag   :: f (TableEntity ItemTagT  ) 
    , _inventoryGroup     :: f (TableEntity GroupT    )
    , _inventoryItemGroup :: f (TableEntity ItemGroupT) }
    deriving (Generic, Database Postgres)

inventoryDb :: DatabaseSettings Postgres InventoryDb
inventoryDb = defaultDbSettings

