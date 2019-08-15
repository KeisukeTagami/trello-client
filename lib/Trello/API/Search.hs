{-
   Trello

   This document describes the REST API of Trello as published by Trello.com.  - <a href='https://trello.com/docs/index.html' target='_blank'>Official Documentation</a>  - <a href='https://trello.com/docs/api' target='_blank'>The HTML pages that were scraped in order to generate this specification.</a>

   OpenAPI spec version: 2.0
   Trello API version: 1.0
   Generated by Swagger Codegen (https://github.com/swagger-api/swagger-codegen.git)
-}

{-|
Module : Trello.API.Search
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Trello.API.Search where

import Trello.Core
import Trello.MimeTypes
import Trello.Model as M

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Data as P (Typeable, TypeRep, typeOf, typeRep)
import qualified Data.Foldable as P
import qualified Data.Map as Map
import qualified Data.Maybe as P
import qualified Data.Proxy as P (Proxy(..))
import qualified Data.Set as Set
import qualified Data.String as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time as TI
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Media as ME
import qualified Network.HTTP.Types as NH
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH

import Data.Text (Text)
import GHC.Base ((<|>))

import Prelude ((==),(/=),($), (.),(<$>),(<*>),(>>=),Maybe(..),Bool(..),Char,Double,FilePath,Float,Int,Integer,String,fmap,undefined,mempty,maybe,pure,Monad,Applicative,Functor)
import qualified Prelude as P

-- * Operations


-- ** Search

-- *** getSearch

-- | @GET \/search@
-- 
-- getSearch()
-- 
getSearch 
  :: Query -- ^ "query" -  a string with a length from 1 to 16384
  -> IdOrganizations -- ^ "idOrganizations" -  A comma-separated list of objectIds, 24-character hex strings
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetSearch MimeNoContent NoContent MimeNoContent
getSearch (Query query) (IdOrganizations idOrganizations) (Key key) (Token token) =
  _mkRequest "GET" ["/search"]
    `setQuery` toQuery ("query", Just query)
    `setQuery` toQuery ("idOrganizations", Just idOrganizations)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetSearch  

-- | /Optional Param/ "idBoards" - A comma-separated list of objectIds, 24-character hex strings
instance HasOptionalParam GetSearch IdBoards where
  applyOptionalParam req (IdBoards xs) =
    req `setQuery` toQuery ("idBoards", Just xs)

-- | /Optional Param/ "idCards" - A comma-separated list of objectIds, 24-character hex strings
instance HasOptionalParam GetSearch IdCards where
  applyOptionalParam req (IdCards xs) =
    req `setQuery` toQuery ("idCards", Just xs)

-- | /Optional Param/ "modelTypes" - all or a comma-separated list of: actions, boards, cards, members or organizations
instance HasOptionalParam GetSearch ModelTypes where
  applyOptionalParam req (ModelTypes xs) =
    req `setQuery` toQuery ("modelTypes", Just xs)

-- | /Optional Param/ "board_fields" - all or a comma-separated list of: closed, dateLastActivity, dateLastView, desc, descData, idOrganization, invitations, invited, labelNames, memberships, name, pinned, powerUps, prefs, shortLink, shortUrl, starred, subscribed or url
instance HasOptionalParam GetSearch BoardFields where
  applyOptionalParam req (BoardFields xs) =
    req `setQuery` toQuery ("board_fields", Just xs)

-- | /Optional Param/ "boards_limit" - a number from 1 to 1000
instance HasOptionalParam GetSearch BoardsLimit where
  applyOptionalParam req (BoardsLimit xs) =
    req `setQuery` toQuery ("boards_limit", Just xs)

-- | /Optional Param/ "card_fields" - all or a comma-separated list of: badges, checkItemStates, closed, dateLastActivity, desc, descData, due, email, idAttachmentCover, idBoard, idChecklists, idLabels, idList, idMembers, idMembersVoted, idShort, labels, manualCoverAttachment, name, pos, shortLink, shortUrl, subscribed or url
instance HasOptionalParam GetSearch CardFields where
  applyOptionalParam req (CardFields xs) =
    req `setQuery` toQuery ("card_fields", Just xs)

-- | /Optional Param/ "cards_limit" - a number from 1 to 1000
instance HasOptionalParam GetSearch CardsLimit where
  applyOptionalParam req (CardsLimit xs) =
    req `setQuery` toQuery ("cards_limit", Just xs)

-- | /Optional Param/ "cards_page" - a number from 0 to 100
instance HasOptionalParam GetSearch CardsPage where
  applyOptionalParam req (CardsPage xs) =
    req `setQuery` toQuery ("cards_page", Just xs)

-- | /Optional Param/ "card_board" -  true or false
instance HasOptionalParam GetSearch CardBoard where
  applyOptionalParam req (CardBoard xs) =
    req `setQuery` toQuery ("card_board", Just xs)

-- | /Optional Param/ "card_list" -  true or false
instance HasOptionalParam GetSearch CardList where
  applyOptionalParam req (CardList xs) =
    req `setQuery` toQuery ("card_list", Just xs)

-- | /Optional Param/ "card_members" -  true or false
instance HasOptionalParam GetSearch CardMembers where
  applyOptionalParam req (CardMembers xs) =
    req `setQuery` toQuery ("card_members", Just xs)

-- | /Optional Param/ "card_stickers" -  true or false
instance HasOptionalParam GetSearch CardStickers where
  applyOptionalParam req (CardStickers xs) =
    req `setQuery` toQuery ("card_stickers", Just xs)

-- | /Optional Param/ "card_attachments" - A boolean value or &quot;cover&quot; for only card cover attachments
instance HasOptionalParam GetSearch CardAttachments where
  applyOptionalParam req (CardAttachments xs) =
    req `setQuery` toQuery ("card_attachments", Just xs)

-- | /Optional Param/ "organization_fields" - all or a comma-separated list of: billableMemberCount, desc, descData, displayName, idBoards, invitations, invited, logoHash, memberships, name, powerUps, prefs, premiumFeatures, products, url or website
instance HasOptionalParam GetSearch OrganizationFields where
  applyOptionalParam req (OrganizationFields xs) =
    req `setQuery` toQuery ("organization_fields", Just xs)

-- | /Optional Param/ "organizations_limit" - a number from 1 to 1000
instance HasOptionalParam GetSearch OrganizationsLimit where
  applyOptionalParam req (OrganizationsLimit xs) =
    req `setQuery` toQuery ("organizations_limit", Just xs)

-- | /Optional Param/ "member_fields" - all or a comma-separated list of: avatarHash, bio, bioData, confirmed, fullName, idPremOrgsAdmin, initials, memberType, products, status, url or username
instance HasOptionalParam GetSearch MemberFields where
  applyOptionalParam req (MemberFields xs) =
    req `setQuery` toQuery ("member_fields", Just xs)

-- | /Optional Param/ "members_limit" - a number from 1 to 1000
instance HasOptionalParam GetSearch MembersLimit where
  applyOptionalParam req (MembersLimit xs) =
    req `setQuery` toQuery ("members_limit", Just xs)

-- | /Optional Param/ "partial" -  true or false
instance HasOptionalParam GetSearch Partial where
  applyOptionalParam req (Partial xs) =
    req `setQuery` toQuery ("partial", Just xs)

-- *** getSearchMembers

-- | @GET \/search\/members@
-- 
-- getSearchMembers()
-- 
getSearchMembers 
  :: Query -- ^ "query" -  a string with a length from 1 to 16384
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetSearchMembers MimeNoContent NoContent MimeNoContent
getSearchMembers (Query query) (Key key) (Token token) =
  _mkRequest "GET" ["/search/members"]
    `setQuery` toQuery ("query", Just query)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetSearchMembers  

-- | /Optional Param/ "limit" - a number from 1 to 20
instance HasOptionalParam GetSearchMembers Limit where
  applyOptionalParam req (Limit xs) =
    req `setQuery` toQuery ("limit", Just xs)

-- | /Optional Param/ "idBoard" - An id, or null
instance HasOptionalParam GetSearchMembers IdBoard where
  applyOptionalParam req (IdBoard xs) =
    req `setQuery` toQuery ("idBoard", Just xs)

-- | /Optional Param/ "idOrganization" - An id, or null
instance HasOptionalParam GetSearchMembers IdOrganization where
  applyOptionalParam req (IdOrganization xs) =
    req `setQuery` toQuery ("idOrganization", Just xs)

-- | /Optional Param/ "onlyOrgMembers" - A boolean
instance HasOptionalParam GetSearchMembers OnlyOrgMembers where
  applyOptionalParam req (OnlyOrgMembers xs) =
    req `setQuery` toQuery ("onlyOrgMembers", Just xs)
