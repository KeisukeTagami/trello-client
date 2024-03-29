{-
   Trello

   This document describes the REST API of Trello as published by Trello.com.  - <a href='https://trello.com/docs/index.html' target='_blank'>Official Documentation</a>  - <a href='https://trello.com/docs/api' target='_blank'>The HTML pages that were scraped in order to generate this specification.</a>

   OpenAPI spec version: 2.0
   Trello API version: 1.0
   Generated by Swagger Codegen (https://github.com/swagger-api/swagger-codegen.git)
-}

{-|
Module : Trello.API.Action
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Trello.API.Action where

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


-- ** Action

-- *** deleteActionsByIdAction

-- | @DELETE \/actions\/{idAction}@
-- 
-- deleteActionsByIdAction()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
deleteActionsByIdAction 
  :: IdAction -- ^ "idAction" -  idAction
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest DeleteActionsByIdAction MimeNoContent NoContent MimeNoContent
deleteActionsByIdAction (IdAction idAction) (Key key) (Token token) =
  _mkRequest "DELETE" ["/actions/",toPath idAction]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data DeleteActionsByIdAction  

-- *** getActionsBoardByIdAction

-- | @GET \/actions\/{idAction}\/board@
-- 
-- getActionsBoardByIdAction()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
getActionsBoardByIdAction 
  :: IdAction -- ^ "idAction" -  idAction
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetActionsBoardByIdAction MimeNoContent NoContent MimeNoContent
getActionsBoardByIdAction (IdAction idAction) (Key key) (Token token) =
  _mkRequest "GET" ["/actions/",toPath idAction,"/board"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetActionsBoardByIdAction  

-- | /Optional Param/ "fields" - all or a comma-separated list of: closed, dateLastActivity, dateLastView, desc, descData, idOrganization, invitations, invited, labelNames, memberships, name, pinned, powerUps, prefs, shortLink, shortUrl, starred, subscribed or url
instance HasOptionalParam GetActionsBoardByIdAction Fields where
  applyOptionalParam req (Fields xs) =
    req `setQuery` toQuery ("fields", Just xs)

-- *** getActionsBoardByIdActionByField

-- | @GET \/actions\/{idAction}\/board\/{field}@
-- 
-- getActionsBoardByIdActionByField()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
getActionsBoardByIdActionByField 
  :: IdAction -- ^ "idAction" -  idAction
  -> Field -- ^ "field" -  field
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetActionsBoardByIdActionByField MimeNoContent NoContent MimeNoContent
getActionsBoardByIdActionByField (IdAction idAction) (Field field) (Key key) (Token token) =
  _mkRequest "GET" ["/actions/",toPath idAction,"/board/",toPath field]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetActionsBoardByIdActionByField  

-- *** getActionsByIdAction

-- | @GET \/actions\/{idAction}@
-- 
-- getActionsByIdAction()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
getActionsByIdAction 
  :: IdAction -- ^ "idAction" -  idAction
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetActionsByIdAction MimeNoContent NoContent MimeNoContent
getActionsByIdAction (IdAction idAction) (Key key) (Token token) =
  _mkRequest "GET" ["/actions/",toPath idAction]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetActionsByIdAction  

-- | /Optional Param/ "display" -  true or false
instance HasOptionalParam GetActionsByIdAction Display where
  applyOptionalParam req (Display xs) =
    req `setQuery` toQuery ("display", Just xs)

-- | /Optional Param/ "entities" -  true or false
instance HasOptionalParam GetActionsByIdAction Entities where
  applyOptionalParam req (Entities xs) =
    req `setQuery` toQuery ("entities", Just xs)

-- | /Optional Param/ "fields" - all or a comma-separated list of: data, date, idMemberCreator or type
instance HasOptionalParam GetActionsByIdAction Fields where
  applyOptionalParam req (Fields xs) =
    req `setQuery` toQuery ("fields", Just xs)

-- | /Optional Param/ "member" -  true or false
instance HasOptionalParam GetActionsByIdAction Member where
  applyOptionalParam req (Member xs) =
    req `setQuery` toQuery ("member", Just xs)

-- | /Optional Param/ "member_fields" - all or a comma-separated list of: avatarHash, bio, bioData, confirmed, fullName, idPremOrgsAdmin, initials, memberType, products, status, url or username
instance HasOptionalParam GetActionsByIdAction MemberFields where
  applyOptionalParam req (MemberFields xs) =
    req `setQuery` toQuery ("member_fields", Just xs)

-- | /Optional Param/ "memberCreator" -  true or false
instance HasOptionalParam GetActionsByIdAction MemberCreator where
  applyOptionalParam req (MemberCreator xs) =
    req `setQuery` toQuery ("memberCreator", Just xs)

-- | /Optional Param/ "memberCreator_fields" - all or a comma-separated list of: avatarHash, bio, bioData, confirmed, fullName, idPremOrgsAdmin, initials, memberType, products, status, url or username
instance HasOptionalParam GetActionsByIdAction MemberCreatorFields where
  applyOptionalParam req (MemberCreatorFields xs) =
    req `setQuery` toQuery ("memberCreator_fields", Just xs)

-- *** getActionsByIdActionByField

-- | @GET \/actions\/{idAction}\/{field}@
-- 
-- getActionsByIdActionByField()
-- 
getActionsByIdActionByField 
  :: IdAction -- ^ "idAction" -  idAction
  -> Field -- ^ "field" -  field
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetActionsByIdActionByField MimeNoContent NoContent MimeNoContent
getActionsByIdActionByField (IdAction idAction) (Field field) (Key key) (Token token) =
  _mkRequest "GET" ["/actions/",toPath idAction,"/",toPath field]
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetActionsByIdActionByField  

-- *** getActionsCardByIdAction

-- | @GET \/actions\/{idAction}\/card@
-- 
-- getActionsCardByIdAction()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
getActionsCardByIdAction 
  :: IdAction -- ^ "idAction" -  idAction
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetActionsCardByIdAction MimeNoContent NoContent MimeNoContent
getActionsCardByIdAction (IdAction idAction) (Key key) (Token token) =
  _mkRequest "GET" ["/actions/",toPath idAction,"/card"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetActionsCardByIdAction  

-- | /Optional Param/ "fields" - all or a comma-separated list of: badges, checkItemStates, closed, dateLastActivity, desc, descData, due, email, idAttachmentCover, idBoard, idChecklists, idLabels, idList, idMembers, idMembersVoted, idShort, labels, manualCoverAttachment, name, pos, shortLink, shortUrl, subscribed or url
instance HasOptionalParam GetActionsCardByIdAction Fields where
  applyOptionalParam req (Fields xs) =
    req `setQuery` toQuery ("fields", Just xs)

-- *** getActionsCardByIdActionByField

-- | @GET \/actions\/{idAction}\/card\/{field}@
-- 
-- getActionsCardByIdActionByField()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
getActionsCardByIdActionByField 
  :: IdAction -- ^ "idAction" -  idAction
  -> Field -- ^ "field" -  field
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetActionsCardByIdActionByField MimeNoContent NoContent MimeNoContent
getActionsCardByIdActionByField (IdAction idAction) (Field field) (Key key) (Token token) =
  _mkRequest "GET" ["/actions/",toPath idAction,"/card/",toPath field]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetActionsCardByIdActionByField  

-- *** getActionsDisplayByIdAction

-- | @GET \/actions\/{idAction}\/display@
-- 
-- getActionsDisplayByIdAction()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
getActionsDisplayByIdAction 
  :: IdAction -- ^ "idAction" -  idAction
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetActionsDisplayByIdAction MimeNoContent NoContent MimeNoContent
getActionsDisplayByIdAction (IdAction idAction) (Key key) (Token token) =
  _mkRequest "GET" ["/actions/",toPath idAction,"/display"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetActionsDisplayByIdAction  

-- *** getActionsEntitiesByIdAction

-- | @GET \/actions\/{idAction}\/entities@
-- 
-- getActionsEntitiesByIdAction()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
getActionsEntitiesByIdAction 
  :: IdAction -- ^ "idAction" -  idAction
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetActionsEntitiesByIdAction MimeNoContent NoContent MimeNoContent
getActionsEntitiesByIdAction (IdAction idAction) (Key key) (Token token) =
  _mkRequest "GET" ["/actions/",toPath idAction,"/entities"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetActionsEntitiesByIdAction  

-- *** getActionsListByIdAction

-- | @GET \/actions\/{idAction}\/list@
-- 
-- getActionsListByIdAction()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
getActionsListByIdAction 
  :: IdAction -- ^ "idAction" -  idAction
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetActionsListByIdAction MimeNoContent NoContent MimeNoContent
getActionsListByIdAction (IdAction idAction) (Key key) (Token token) =
  _mkRequest "GET" ["/actions/",toPath idAction,"/list"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetActionsListByIdAction  

-- | /Optional Param/ "fields" - all or a comma-separated list of: closed, idBoard, name, pos or subscribed
instance HasOptionalParam GetActionsListByIdAction Fields where
  applyOptionalParam req (Fields xs) =
    req `setQuery` toQuery ("fields", Just xs)

-- *** getActionsListByIdActionByField

-- | @GET \/actions\/{idAction}\/list\/{field}@
-- 
-- getActionsListByIdActionByField()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
getActionsListByIdActionByField 
  :: IdAction -- ^ "idAction" -  idAction
  -> Field -- ^ "field" -  field
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetActionsListByIdActionByField MimeNoContent NoContent MimeNoContent
getActionsListByIdActionByField (IdAction idAction) (Field field) (Key key) (Token token) =
  _mkRequest "GET" ["/actions/",toPath idAction,"/list/",toPath field]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetActionsListByIdActionByField  

-- *** getActionsMemberByIdAction

-- | @GET \/actions\/{idAction}\/member@
-- 
-- getActionsMemberByIdAction()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
getActionsMemberByIdAction 
  :: IdAction -- ^ "idAction" -  idAction
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetActionsMemberByIdAction MimeNoContent NoContent MimeNoContent
getActionsMemberByIdAction (IdAction idAction) (Key key) (Token token) =
  _mkRequest "GET" ["/actions/",toPath idAction,"/member"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetActionsMemberByIdAction  

-- | /Optional Param/ "fields" - all or a comma-separated list of: avatarHash, avatarSource, bio, bioData, confirmed, email, fullName, gravatarHash, idBoards, idBoardsPinned, idOrganizations, idPremOrgsAdmin, initials, loginTypes, memberType, oneTimeMessagesDismissed, prefs, premiumFeatures, products, status, status, trophies, uploadedAvatarHash, url or username
instance HasOptionalParam GetActionsMemberByIdAction Fields where
  applyOptionalParam req (Fields xs) =
    req `setQuery` toQuery ("fields", Just xs)

-- *** getActionsMemberByIdActionByField

-- | @GET \/actions\/{idAction}\/member\/{field}@
-- 
-- getActionsMemberByIdActionByField()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
getActionsMemberByIdActionByField 
  :: IdAction -- ^ "idAction" -  idAction
  -> Field -- ^ "field" -  field
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetActionsMemberByIdActionByField MimeNoContent NoContent MimeNoContent
getActionsMemberByIdActionByField (IdAction idAction) (Field field) (Key key) (Token token) =
  _mkRequest "GET" ["/actions/",toPath idAction,"/member/",toPath field]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetActionsMemberByIdActionByField  

-- *** getActionsMemberCreatorByIdAction

-- | @GET \/actions\/{idAction}\/memberCreator@
-- 
-- getActionsMemberCreatorByIdAction()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
getActionsMemberCreatorByIdAction 
  :: IdAction -- ^ "idAction" -  idAction
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetActionsMemberCreatorByIdAction MimeNoContent NoContent MimeNoContent
getActionsMemberCreatorByIdAction (IdAction idAction) (Key key) (Token token) =
  _mkRequest "GET" ["/actions/",toPath idAction,"/memberCreator"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetActionsMemberCreatorByIdAction  

-- | /Optional Param/ "fields" - all or a comma-separated list of: avatarHash, avatarSource, bio, bioData, confirmed, email, fullName, gravatarHash, idBoards, idBoardsPinned, idOrganizations, idPremOrgsAdmin, initials, loginTypes, memberType, oneTimeMessagesDismissed, prefs, premiumFeatures, products, status, status, trophies, uploadedAvatarHash, url or username
instance HasOptionalParam GetActionsMemberCreatorByIdAction Fields where
  applyOptionalParam req (Fields xs) =
    req `setQuery` toQuery ("fields", Just xs)

-- *** getActionsMemberCreatorByIdActionByField

-- | @GET \/actions\/{idAction}\/memberCreator\/{field}@
-- 
-- getActionsMemberCreatorByIdActionByField()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
getActionsMemberCreatorByIdActionByField 
  :: IdAction -- ^ "idAction" -  idAction
  -> Field -- ^ "field" -  field
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetActionsMemberCreatorByIdActionByField MimeNoContent NoContent MimeNoContent
getActionsMemberCreatorByIdActionByField (IdAction idAction) (Field field) (Key key) (Token token) =
  _mkRequest "GET" ["/actions/",toPath idAction,"/memberCreator/",toPath field]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetActionsMemberCreatorByIdActionByField  

-- *** getActionsOrganizationByIdAction

-- | @GET \/actions\/{idAction}\/organization@
-- 
-- getActionsOrganizationByIdAction()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
getActionsOrganizationByIdAction 
  :: IdAction -- ^ "idAction" -  idAction
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetActionsOrganizationByIdAction MimeNoContent NoContent MimeNoContent
getActionsOrganizationByIdAction (IdAction idAction) (Key key) (Token token) =
  _mkRequest "GET" ["/actions/",toPath idAction,"/organization"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetActionsOrganizationByIdAction  

-- | /Optional Param/ "fields" - all or a comma-separated list of: billableMemberCount, desc, descData, displayName, idBoards, invitations, invited, logoHash, memberships, name, powerUps, prefs, premiumFeatures, products, url or website
instance HasOptionalParam GetActionsOrganizationByIdAction Fields where
  applyOptionalParam req (Fields xs) =
    req `setQuery` toQuery ("fields", Just xs)

-- *** getActionsOrganizationByIdActionByField

-- | @GET \/actions\/{idAction}\/organization\/{field}@
-- 
-- getActionsOrganizationByIdActionByField()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
getActionsOrganizationByIdActionByField 
  :: IdAction -- ^ "idAction" -  idAction
  -> Field -- ^ "field" -  field
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetActionsOrganizationByIdActionByField MimeNoContent NoContent MimeNoContent
getActionsOrganizationByIdActionByField (IdAction idAction) (Field field) (Key key) (Token token) =
  _mkRequest "GET" ["/actions/",toPath idAction,"/organization/",toPath field]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetActionsOrganizationByIdActionByField  

-- *** updateActionsByIdAction

-- | @PUT \/actions\/{idAction}@
-- 
-- updateActionsByIdAction()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
updateActionsByIdAction 
  :: (Consumes UpdateActionsByIdAction contentType, MimeRender contentType Actions)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> IdAction -- ^ "idAction" -  idAction
  -> Actions -- ^ "body" -  Attributes of \"Actions\" to be updated.
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest UpdateActionsByIdAction contentType NoContent MimeNoContent
updateActionsByIdAction _ (IdAction idAction) body (Key key) (Token token) =
  _mkRequest "PUT" ["/actions/",toPath idAction]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` body
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data UpdateActionsByIdAction 

-- | /Body Param/ "body" - Attributes of \"Actions\" to be updated.
instance HasBodyParam UpdateActionsByIdAction Actions 

-- *** updateActionsTextByIdAction

-- | @PUT \/actions\/{idAction}\/text@
-- 
-- updateActionsTextByIdAction()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
updateActionsTextByIdAction 
  :: (Consumes UpdateActionsTextByIdAction contentType, MimeRender contentType ActionsText)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> IdAction -- ^ "idAction" -  idAction
  -> ActionsText -- ^ "body" -  Attributes of \"Actions Text\" to be updated.
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest UpdateActionsTextByIdAction contentType NoContent MimeNoContent
updateActionsTextByIdAction _ (IdAction idAction) body (Key key) (Token token) =
  _mkRequest "PUT" ["/actions/",toPath idAction,"/text"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` body
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data UpdateActionsTextByIdAction 

-- | /Body Param/ "body" - Attributes of \"Actions Text\" to be updated.
instance HasBodyParam UpdateActionsTextByIdAction ActionsText 
