{-
   Trello

   This document describes the REST API of Trello as published by Trello.com.  - <a href='https://trello.com/docs/index.html' target='_blank'>Official Documentation</a>  - <a href='https://trello.com/docs/api' target='_blank'>The HTML pages that were scraped in order to generate this specification.</a>

   OpenAPI spec version: 2.0
   Trello API version: 1.0
   Generated by Swagger Codegen (https://github.com/swagger-api/swagger-codegen.git)
-}

{-|
Module : Trello.API.Checklist
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Trello.API.Checklist where

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


-- ** Checklist

-- *** addChecklists

-- | @POST \/checklists@
-- 
-- addChecklists()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
addChecklists 
  :: (Consumes AddChecklists contentType, MimeRender contentType Checklists)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Checklists -- ^ "body" -  Attributes of \"Checklists\" to be added.
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest AddChecklists contentType NoContent MimeNoContent
addChecklists _ body (Key key) (Token token) =
  _mkRequest "POST" ["/checklists"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` body
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data AddChecklists 

-- | /Body Param/ "body" - Attributes of \"Checklists\" to be added.
instance HasBodyParam AddChecklists Checklists 

-- *** addChecklistsCheckItemsByIdChecklist

-- | @POST \/checklists\/{idChecklist}\/checkItems@
-- 
-- addChecklistsCheckItemsByIdChecklist()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
addChecklistsCheckItemsByIdChecklist 
  :: (Consumes AddChecklistsCheckItemsByIdChecklist contentType, MimeRender contentType ChecklistsCheckItems)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> IdChecklist -- ^ "idChecklist" -  idChecklist
  -> ChecklistsCheckItems -- ^ "body" -  Attributes of \"Checklists Check Items\" to be added.
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest AddChecklistsCheckItemsByIdChecklist contentType NoContent MimeNoContent
addChecklistsCheckItemsByIdChecklist _ (IdChecklist idChecklist) body (Key key) (Token token) =
  _mkRequest "POST" ["/checklists/",toPath idChecklist,"/checkItems"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` body
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data AddChecklistsCheckItemsByIdChecklist 

-- | /Body Param/ "body" - Attributes of \"Checklists Check Items\" to be added.
instance HasBodyParam AddChecklistsCheckItemsByIdChecklist ChecklistsCheckItems 

-- *** deleteChecklistsByIdChecklist

-- | @DELETE \/checklists\/{idChecklist}@
-- 
-- deleteChecklistsByIdChecklist()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
deleteChecklistsByIdChecklist 
  :: IdChecklist -- ^ "idChecklist" -  idChecklist
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest DeleteChecklistsByIdChecklist MimeNoContent NoContent MimeNoContent
deleteChecklistsByIdChecklist (IdChecklist idChecklist) (Key key) (Token token) =
  _mkRequest "DELETE" ["/checklists/",toPath idChecklist]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data DeleteChecklistsByIdChecklist  

-- *** deleteChecklistsCheckItemsByIdChecklistByIdCheckItem

-- | @DELETE \/checklists\/{idChecklist}\/checkItems\/{idCheckItem}@
-- 
-- deleteChecklistsCheckItemsByIdChecklistByIdCheckItem()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
deleteChecklistsCheckItemsByIdChecklistByIdCheckItem 
  :: IdChecklist -- ^ "idChecklist" -  idChecklist
  -> IdCheckItem -- ^ "idCheckItem" -  idCheckItem
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest DeleteChecklistsCheckItemsByIdChecklistByIdCheckItem MimeNoContent NoContent MimeNoContent
deleteChecklistsCheckItemsByIdChecklistByIdCheckItem (IdChecklist idChecklist) (IdCheckItem idCheckItem) (Key key) (Token token) =
  _mkRequest "DELETE" ["/checklists/",toPath idChecklist,"/checkItems/",toPath idCheckItem]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data DeleteChecklistsCheckItemsByIdChecklistByIdCheckItem  

-- *** getChecklistsBoardByIdChecklist

-- | @GET \/checklists\/{idChecklist}\/board@
-- 
-- getChecklistsBoardByIdChecklist()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
getChecklistsBoardByIdChecklist 
  :: IdChecklist -- ^ "idChecklist" -  idChecklist
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetChecklistsBoardByIdChecklist MimeNoContent NoContent MimeNoContent
getChecklistsBoardByIdChecklist (IdChecklist idChecklist) (Key key) (Token token) =
  _mkRequest "GET" ["/checklists/",toPath idChecklist,"/board"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetChecklistsBoardByIdChecklist  

-- | /Optional Param/ "fields" - all or a comma-separated list of: closed, dateLastActivity, dateLastView, desc, descData, idOrganization, invitations, invited, labelNames, memberships, name, pinned, powerUps, prefs, shortLink, shortUrl, starred, subscribed or url
instance HasOptionalParam GetChecklistsBoardByIdChecklist Fields where
  applyOptionalParam req (Fields xs) =
    req `setQuery` toQuery ("fields", Just xs)

-- *** getChecklistsBoardByIdChecklistByField

-- | @GET \/checklists\/{idChecklist}\/board\/{field}@
-- 
-- getChecklistsBoardByIdChecklistByField()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
getChecklistsBoardByIdChecklistByField 
  :: IdChecklist -- ^ "idChecklist" -  idChecklist
  -> Field -- ^ "field" -  field
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetChecklistsBoardByIdChecklistByField MimeNoContent NoContent MimeNoContent
getChecklistsBoardByIdChecklistByField (IdChecklist idChecklist) (Field field) (Key key) (Token token) =
  _mkRequest "GET" ["/checklists/",toPath idChecklist,"/board/",toPath field]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetChecklistsBoardByIdChecklistByField  

-- *** getChecklistsByIdChecklist

-- | @GET \/checklists\/{idChecklist}@
-- 
-- getChecklistsByIdChecklist()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
getChecklistsByIdChecklist 
  :: IdChecklist -- ^ "idChecklist" -  idChecklist
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetChecklistsByIdChecklist MimeNoContent NoContent MimeNoContent
getChecklistsByIdChecklist (IdChecklist idChecklist) (Key key) (Token token) =
  _mkRequest "GET" ["/checklists/",toPath idChecklist]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetChecklistsByIdChecklist  

-- | /Optional Param/ "cards" - One of: all, closed, none, open or visible
instance HasOptionalParam GetChecklistsByIdChecklist Cards2 where
  applyOptionalParam req (Cards2 xs) =
    req `setQuery` toQuery ("cards", Just xs)

-- | /Optional Param/ "card_fields" - all or a comma-separated list of: badges, checkItemStates, closed, dateLastActivity, desc, descData, due, email, idAttachmentCover, idBoard, idChecklists, idLabels, idList, idMembers, idMembersVoted, idShort, labels, manualCoverAttachment, name, pos, shortLink, shortUrl, subscribed or url
instance HasOptionalParam GetChecklistsByIdChecklist CardFields where
  applyOptionalParam req (CardFields xs) =
    req `setQuery` toQuery ("card_fields", Just xs)

-- | /Optional Param/ "checkItems" - One of: all or none
instance HasOptionalParam GetChecklistsByIdChecklist CheckItems where
  applyOptionalParam req (CheckItems xs) =
    req `setQuery` toQuery ("checkItems", Just xs)

-- | /Optional Param/ "checkItem_fields" - all or a comma-separated list of: name, nameData, pos, state or type
instance HasOptionalParam GetChecklistsByIdChecklist CheckItemFields where
  applyOptionalParam req (CheckItemFields xs) =
    req `setQuery` toQuery ("checkItem_fields", Just xs)

-- | /Optional Param/ "fields" - all or a comma-separated list of: idBoard, idCard, name or pos
instance HasOptionalParam GetChecklistsByIdChecklist Fields where
  applyOptionalParam req (Fields xs) =
    req `setQuery` toQuery ("fields", Just xs)

-- *** getChecklistsByIdChecklistByField

-- | @GET \/checklists\/{idChecklist}\/{field}@
-- 
-- getChecklistsByIdChecklistByField()
-- 
getChecklistsByIdChecklistByField 
  :: IdChecklist -- ^ "idChecklist" -  idChecklist
  -> Field -- ^ "field" -  field
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetChecklistsByIdChecklistByField MimeNoContent NoContent MimeNoContent
getChecklistsByIdChecklistByField (IdChecklist idChecklist) (Field field) (Key key) (Token token) =
  _mkRequest "GET" ["/checklists/",toPath idChecklist,"/",toPath field]
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetChecklistsByIdChecklistByField  

-- *** getChecklistsCardsByIdChecklist

-- | @GET \/checklists\/{idChecklist}\/cards@
-- 
-- getChecklistsCardsByIdChecklist()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
getChecklistsCardsByIdChecklist 
  :: IdChecklist -- ^ "idChecklist" -  idChecklist
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetChecklistsCardsByIdChecklist MimeNoContent NoContent MimeNoContent
getChecklistsCardsByIdChecklist (IdChecklist idChecklist) (Key key) (Token token) =
  _mkRequest "GET" ["/checklists/",toPath idChecklist,"/cards"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetChecklistsCardsByIdChecklist  

-- | /Optional Param/ "actions" - all or a comma-separated list of: addAttachmentToCard, addChecklistToCard, addMemberToBoard, addMemberToCard, addMemberToOrganization, addToOrganizationBoard, commentCard, convertToCardFromCheckItem, copyBoard, copyCard, copyCommentCard, createBoard, createCard, createList, createOrganization, deleteAttachmentFromCard, deleteBoardInvitation, deleteCard, deleteOrganizationInvitation, disablePowerUp, emailCard, enablePowerUp, makeAdminOfBoard, makeNormalMemberOfBoard, makeNormalMemberOfOrganization, makeObserverOfBoard, memberJoinedTrello, moveCardFromBoard, moveCardToBoard, moveListFromBoard, moveListToBoard, removeChecklistFromCard, removeFromOrganizationBoard, removeMemberFromCard, unconfirmedBoardInvitation, unconfirmedOrganizationInvitation, updateBoard, updateCard, updateCard:closed, updateCard:desc, updateCard:idList, updateCard:name, updateCheckItemStateOnCard, updateChecklist, updateList, updateList:closed, updateList:name, updateMember or updateOrganization
instance HasOptionalParam GetChecklistsCardsByIdChecklist Actions2 where
  applyOptionalParam req (Actions2 xs) =
    req `setQuery` toQuery ("actions", Just xs)

-- | /Optional Param/ "attachments" - A boolean value or &quot;cover&quot; for only card cover attachments
instance HasOptionalParam GetChecklistsCardsByIdChecklist Attachments where
  applyOptionalParam req (Attachments xs) =
    req `setQuery` toQuery ("attachments", Just xs)

-- | /Optional Param/ "attachment_fields" - all or a comma-separated list of: bytes, date, edgeColor, idMember, isUpload, mimeType, name, previews or url
instance HasOptionalParam GetChecklistsCardsByIdChecklist AttachmentFields where
  applyOptionalParam req (AttachmentFields xs) =
    req `setQuery` toQuery ("attachment_fields", Just xs)

-- | /Optional Param/ "stickers" -  true or false
instance HasOptionalParam GetChecklistsCardsByIdChecklist Stickers where
  applyOptionalParam req (Stickers xs) =
    req `setQuery` toQuery ("stickers", Just xs)

-- | /Optional Param/ "members" -  true or false
instance HasOptionalParam GetChecklistsCardsByIdChecklist Members2 where
  applyOptionalParam req (Members2 xs) =
    req `setQuery` toQuery ("members", Just xs)

-- | /Optional Param/ "member_fields" - all or a comma-separated list of: avatarHash, bio, bioData, confirmed, fullName, idPremOrgsAdmin, initials, memberType, products, status, url or username
instance HasOptionalParam GetChecklistsCardsByIdChecklist MemberFields where
  applyOptionalParam req (MemberFields xs) =
    req `setQuery` toQuery ("member_fields", Just xs)

-- | /Optional Param/ "checkItemStates" -  true or false
instance HasOptionalParam GetChecklistsCardsByIdChecklist CheckItemStates where
  applyOptionalParam req (CheckItemStates xs) =
    req `setQuery` toQuery ("checkItemStates", Just xs)

-- | /Optional Param/ "checklists" - One of: all or none
instance HasOptionalParam GetChecklistsCardsByIdChecklist Checklists2 where
  applyOptionalParam req (Checklists2 xs) =
    req `setQuery` toQuery ("checklists", Just xs)

-- | /Optional Param/ "limit" - a number from 1 to 1000
instance HasOptionalParam GetChecklistsCardsByIdChecklist Limit where
  applyOptionalParam req (Limit xs) =
    req `setQuery` toQuery ("limit", Just xs)

-- | /Optional Param/ "since" - A date, or null
instance HasOptionalParam GetChecklistsCardsByIdChecklist Since where
  applyOptionalParam req (Since xs) =
    req `setQuery` toQuery ("since", Just xs)

-- | /Optional Param/ "before" - A date, or null
instance HasOptionalParam GetChecklistsCardsByIdChecklist Before where
  applyOptionalParam req (Before xs) =
    req `setQuery` toQuery ("before", Just xs)

-- | /Optional Param/ "filter" - One of: all, closed, none or open
instance HasOptionalParam GetChecklistsCardsByIdChecklist Filter where
  applyOptionalParam req (Filter xs) =
    req `setQuery` toQuery ("filter", Just xs)

-- | /Optional Param/ "fields" - all or a comma-separated list of: badges, checkItemStates, closed, dateLastActivity, desc, descData, due, email, idAttachmentCover, idBoard, idChecklists, idLabels, idList, idMembers, idMembersVoted, idShort, labels, manualCoverAttachment, name, pos, shortLink, shortUrl, subscribed or url
instance HasOptionalParam GetChecklistsCardsByIdChecklist Fields where
  applyOptionalParam req (Fields xs) =
    req `setQuery` toQuery ("fields", Just xs)

-- *** getChecklistsCardsByIdChecklistByFilter

-- | @GET \/checklists\/{idChecklist}\/cards\/{filter}@
-- 
-- getChecklistsCardsByIdChecklistByFilter()
-- 
getChecklistsCardsByIdChecklistByFilter 
  :: IdChecklist -- ^ "idChecklist" -  idChecklist
  -> Filter -- ^ "filter" -  filter
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetChecklistsCardsByIdChecklistByFilter MimeNoContent NoContent MimeNoContent
getChecklistsCardsByIdChecklistByFilter (IdChecklist idChecklist) (Filter filter) (Key key) (Token token) =
  _mkRequest "GET" ["/checklists/",toPath idChecklist,"/cards/",toPath filter]
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetChecklistsCardsByIdChecklistByFilter  

-- *** getChecklistsCheckItemsByIdChecklist

-- | @GET \/checklists\/{idChecklist}\/checkItems@
-- 
-- getChecklistsCheckItemsByIdChecklist()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
getChecklistsCheckItemsByIdChecklist 
  :: IdChecklist -- ^ "idChecklist" -  idChecklist
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetChecklistsCheckItemsByIdChecklist MimeNoContent NoContent MimeNoContent
getChecklistsCheckItemsByIdChecklist (IdChecklist idChecklist) (Key key) (Token token) =
  _mkRequest "GET" ["/checklists/",toPath idChecklist,"/checkItems"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetChecklistsCheckItemsByIdChecklist  

-- | /Optional Param/ "filter" - One of: all or none
instance HasOptionalParam GetChecklistsCheckItemsByIdChecklist Filter where
  applyOptionalParam req (Filter xs) =
    req `setQuery` toQuery ("filter", Just xs)

-- | /Optional Param/ "fields" - all or a comma-separated list of: name, nameData, pos, state or type
instance HasOptionalParam GetChecklistsCheckItemsByIdChecklist Fields where
  applyOptionalParam req (Fields xs) =
    req `setQuery` toQuery ("fields", Just xs)

-- *** getChecklistsCheckItemsByIdChecklistByIdCheckItem

-- | @GET \/checklists\/{idChecklist}\/checkItems\/{idCheckItem}@
-- 
-- getChecklistsCheckItemsByIdChecklistByIdCheckItem()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
getChecklistsCheckItemsByIdChecklistByIdCheckItem 
  :: IdChecklist -- ^ "idChecklist" -  idChecklist
  -> IdCheckItem -- ^ "idCheckItem" -  idCheckItem
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetChecklistsCheckItemsByIdChecklistByIdCheckItem MimeNoContent NoContent MimeNoContent
getChecklistsCheckItemsByIdChecklistByIdCheckItem (IdChecklist idChecklist) (IdCheckItem idCheckItem) (Key key) (Token token) =
  _mkRequest "GET" ["/checklists/",toPath idChecklist,"/checkItems/",toPath idCheckItem]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetChecklistsCheckItemsByIdChecklistByIdCheckItem  

-- | /Optional Param/ "fields" - all or a comma-separated list of: name, nameData, pos, state or type
instance HasOptionalParam GetChecklistsCheckItemsByIdChecklistByIdCheckItem Fields where
  applyOptionalParam req (Fields xs) =
    req `setQuery` toQuery ("fields", Just xs)

-- *** updateChecklistsByIdChecklist

-- | @PUT \/checklists\/{idChecklist}@
-- 
-- updateChecklistsByIdChecklist()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
updateChecklistsByIdChecklist 
  :: (Consumes UpdateChecklistsByIdChecklist contentType, MimeRender contentType Checklists)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> IdChecklist -- ^ "idChecklist" -  idChecklist
  -> Checklists -- ^ "body" -  Attributes of \"Checklists\" to be updated.
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest UpdateChecklistsByIdChecklist contentType NoContent MimeNoContent
updateChecklistsByIdChecklist _ (IdChecklist idChecklist) body (Key key) (Token token) =
  _mkRequest "PUT" ["/checklists/",toPath idChecklist]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` body
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data UpdateChecklistsByIdChecklist 

-- | /Body Param/ "body" - Attributes of \"Checklists\" to be updated.
instance HasBodyParam UpdateChecklistsByIdChecklist Checklists 

-- *** updateChecklistsIdCardByIdChecklist

-- | @PUT \/checklists\/{idChecklist}\/idCard@
-- 
-- updateChecklistsIdCardByIdChecklist()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
updateChecklistsIdCardByIdChecklist 
  :: (Consumes UpdateChecklistsIdCardByIdChecklist contentType, MimeRender contentType ChecklistsIdCard)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> IdChecklist -- ^ "idChecklist" -  idChecklist
  -> ChecklistsIdCard -- ^ "body" -  Attributes of \"Checklists Id Card\" to be updated.
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest UpdateChecklistsIdCardByIdChecklist contentType NoContent MimeNoContent
updateChecklistsIdCardByIdChecklist _ (IdChecklist idChecklist) body (Key key) (Token token) =
  _mkRequest "PUT" ["/checklists/",toPath idChecklist,"/idCard"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` body
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data UpdateChecklistsIdCardByIdChecklist 

-- | /Body Param/ "body" - Attributes of \"Checklists Id Card\" to be updated.
instance HasBodyParam UpdateChecklistsIdCardByIdChecklist ChecklistsIdCard 

-- *** updateChecklistsNameByIdChecklist

-- | @PUT \/checklists\/{idChecklist}\/name@
-- 
-- updateChecklistsNameByIdChecklist()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
updateChecklistsNameByIdChecklist 
  :: (Consumes UpdateChecklistsNameByIdChecklist contentType, MimeRender contentType ChecklistsName)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> IdChecklist -- ^ "idChecklist" -  idChecklist
  -> ChecklistsName -- ^ "body" -  Attributes of \"Checklists Name\" to be updated.
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest UpdateChecklistsNameByIdChecklist contentType NoContent MimeNoContent
updateChecklistsNameByIdChecklist _ (IdChecklist idChecklist) body (Key key) (Token token) =
  _mkRequest "PUT" ["/checklists/",toPath idChecklist,"/name"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` body
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data UpdateChecklistsNameByIdChecklist 

-- | /Body Param/ "body" - Attributes of \"Checklists Name\" to be updated.
instance HasBodyParam UpdateChecklistsNameByIdChecklist ChecklistsName 

-- *** updateChecklistsPosByIdChecklist

-- | @PUT \/checklists\/{idChecklist}\/pos@
-- 
-- updateChecklistsPosByIdChecklist()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
updateChecklistsPosByIdChecklist 
  :: (Consumes UpdateChecklistsPosByIdChecklist contentType, MimeRender contentType ChecklistsPos)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> IdChecklist -- ^ "idChecklist" -  idChecklist
  -> ChecklistsPos -- ^ "body" -  Attributes of \"Checklists Pos\" to be updated.
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest UpdateChecklistsPosByIdChecklist contentType NoContent MimeNoContent
updateChecklistsPosByIdChecklist _ (IdChecklist idChecklist) body (Key key) (Token token) =
  _mkRequest "PUT" ["/checklists/",toPath idChecklist,"/pos"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` body
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data UpdateChecklistsPosByIdChecklist 

-- | /Body Param/ "body" - Attributes of \"Checklists Pos\" to be updated.
instance HasBodyParam UpdateChecklistsPosByIdChecklist ChecklistsPos 
