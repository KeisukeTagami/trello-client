{-
   Trello

   This document describes the REST API of Trello as published by Trello.com.  - <a href='https://trello.com/docs/index.html' target='_blank'>Official Documentation</a>  - <a href='https://trello.com/docs/api' target='_blank'>The HTML pages that were scraped in order to generate this specification.</a>

   OpenAPI spec version: 2.0
   Trello API version: 1.0
   Generated by Swagger Codegen (https://github.com/swagger-api/swagger-codegen.git)
-}

{-|
Module : Trello.API.ApiList
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Trello.API.ApiList where

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


-- ** List

-- *** addLists

-- | @POST \/lists@
-- 
-- addLists()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
addLists 
  :: (Consumes AddLists contentType, MimeRender contentType Lists)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Lists -- ^ "body" -  Attributes of \"Lists\" to be added.
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest AddLists contentType NoContent MimeNoContent
addLists _ body (Key key) (Token token) =
  _mkRequest "POST" ["/lists"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` body
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data AddLists 

-- | /Body Param/ "body" - Attributes of \"Lists\" to be added.
instance HasBodyParam AddLists Lists 

-- *** addListsArchiveAllCardsByIdList

-- | @POST \/lists\/{idList}\/archiveAllCards@
-- 
-- addListsArchiveAllCardsByIdList()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
addListsArchiveAllCardsByIdList 
  :: IdList -- ^ "idList" -  idList
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest AddListsArchiveAllCardsByIdList MimeNoContent NoContent MimeNoContent
addListsArchiveAllCardsByIdList (IdList idList) (Key key) (Token token) =
  _mkRequest "POST" ["/lists/",toPath idList,"/archiveAllCards"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data AddListsArchiveAllCardsByIdList  

-- *** addListsCardsByIdList

-- | @POST \/lists\/{idList}\/cards@
-- 
-- addListsCardsByIdList()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
addListsCardsByIdList 
  :: (Consumes AddListsCardsByIdList contentType, MimeRender contentType ListsCards)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> IdList -- ^ "idList" -  idList
  -> ListsCards -- ^ "body" -  Attributes of \"Lists Cards\" to be added.
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest AddListsCardsByIdList contentType NoContent MimeNoContent
addListsCardsByIdList _ (IdList idList) body (Key key) (Token token) =
  _mkRequest "POST" ["/lists/",toPath idList,"/cards"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` body
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data AddListsCardsByIdList 

-- | /Body Param/ "body" - Attributes of \"Lists Cards\" to be added.
instance HasBodyParam AddListsCardsByIdList ListsCards 

-- *** addListsMoveAllCardsByIdList

-- | @POST \/lists\/{idList}\/moveAllCards@
-- 
-- addListsMoveAllCardsByIdList()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
addListsMoveAllCardsByIdList 
  :: (Consumes AddListsMoveAllCardsByIdList contentType, MimeRender contentType ListsMoveAllCards)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> IdList -- ^ "idList" -  idList
  -> ListsMoveAllCards -- ^ "body" -  Attributes of \"Lists Move All Cards\" to be added.
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest AddListsMoveAllCardsByIdList contentType NoContent MimeNoContent
addListsMoveAllCardsByIdList _ (IdList idList) body (Key key) (Token token) =
  _mkRequest "POST" ["/lists/",toPath idList,"/moveAllCards"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` body
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data AddListsMoveAllCardsByIdList 

-- | /Body Param/ "body" - Attributes of \"Lists Move All Cards\" to be added.
instance HasBodyParam AddListsMoveAllCardsByIdList ListsMoveAllCards 

-- *** getListsActionsByIdList

-- | @GET \/lists\/{idList}\/actions@
-- 
-- getListsActionsByIdList()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
getListsActionsByIdList 
  :: IdList -- ^ "idList" -  idList
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetListsActionsByIdList MimeNoContent NoContent MimeNoContent
getListsActionsByIdList (IdList idList) (Key key) (Token token) =
  _mkRequest "GET" ["/lists/",toPath idList,"/actions"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetListsActionsByIdList  

-- | /Optional Param/ "entities" -  true or false
instance HasOptionalParam GetListsActionsByIdList Entities where
  applyOptionalParam req (Entities xs) =
    req `setQuery` toQuery ("entities", Just xs)

-- | /Optional Param/ "display" -  true or false
instance HasOptionalParam GetListsActionsByIdList Display where
  applyOptionalParam req (Display xs) =
    req `setQuery` toQuery ("display", Just xs)

-- | /Optional Param/ "filter" - all or a comma-separated list of: addAttachmentToCard, addChecklistToCard, addMemberToBoard, addMemberToCard, addMemberToOrganization, addToOrganizationBoard, commentCard, convertToCardFromCheckItem, copyBoard, copyCard, copyCommentCard, createBoard, createCard, createList, createOrganization, deleteAttachmentFromCard, deleteBoardInvitation, deleteCard, deleteOrganizationInvitation, disablePowerUp, emailCard, enablePowerUp, makeAdminOfBoard, makeNormalMemberOfBoard, makeNormalMemberOfOrganization, makeObserverOfBoard, memberJoinedTrello, moveCardFromBoard, moveCardToBoard, moveListFromBoard, moveListToBoard, removeChecklistFromCard, removeFromOrganizationBoard, removeMemberFromCard, unconfirmedBoardInvitation, unconfirmedOrganizationInvitation, updateBoard, updateCard, updateCard:closed, updateCard:desc, updateCard:idList, updateCard:name, updateCheckItemStateOnCard, updateChecklist, updateList, updateList:closed, updateList:name, updateMember or updateOrganization
instance HasOptionalParam GetListsActionsByIdList Filter where
  applyOptionalParam req (Filter xs) =
    req `setQuery` toQuery ("filter", Just xs)

-- | /Optional Param/ "fields" - all or a comma-separated list of: data, date, idMemberCreator or type
instance HasOptionalParam GetListsActionsByIdList Fields where
  applyOptionalParam req (Fields xs) =
    req `setQuery` toQuery ("fields", Just xs)

-- | /Optional Param/ "limit" - a number from 0 to 1000
instance HasOptionalParam GetListsActionsByIdList Limit where
  applyOptionalParam req (Limit xs) =
    req `setQuery` toQuery ("limit", Just xs)

-- | /Optional Param/ "format" - One of: count, list or minimal
instance HasOptionalParam GetListsActionsByIdList Format where
  applyOptionalParam req (Format xs) =
    req `setQuery` toQuery ("format", Just xs)

-- | /Optional Param/ "since" - A date, null or lastView
instance HasOptionalParam GetListsActionsByIdList Since where
  applyOptionalParam req (Since xs) =
    req `setQuery` toQuery ("since", Just xs)

-- | /Optional Param/ "before" - A date, or null
instance HasOptionalParam GetListsActionsByIdList Before where
  applyOptionalParam req (Before xs) =
    req `setQuery` toQuery ("before", Just xs)

-- | /Optional Param/ "page" - Page * limit must be less than 1000
instance HasOptionalParam GetListsActionsByIdList Page where
  applyOptionalParam req (Page xs) =
    req `setQuery` toQuery ("page", Just xs)

-- | /Optional Param/ "idModels" - Only return actions related to these model ids
instance HasOptionalParam GetListsActionsByIdList IdModels where
  applyOptionalParam req (IdModels xs) =
    req `setQuery` toQuery ("idModels", Just xs)

-- | /Optional Param/ "member" -  true or false
instance HasOptionalParam GetListsActionsByIdList Member where
  applyOptionalParam req (Member xs) =
    req `setQuery` toQuery ("member", Just xs)

-- | /Optional Param/ "member_fields" - all or a comma-separated list of: avatarHash, bio, bioData, confirmed, fullName, idPremOrgsAdmin, initials, memberType, products, status, url or username
instance HasOptionalParam GetListsActionsByIdList MemberFields where
  applyOptionalParam req (MemberFields xs) =
    req `setQuery` toQuery ("member_fields", Just xs)

-- | /Optional Param/ "memberCreator" -  true or false
instance HasOptionalParam GetListsActionsByIdList MemberCreator where
  applyOptionalParam req (MemberCreator xs) =
    req `setQuery` toQuery ("memberCreator", Just xs)

-- | /Optional Param/ "memberCreator_fields" - all or a comma-separated list of: avatarHash, bio, bioData, confirmed, fullName, idPremOrgsAdmin, initials, memberType, products, status, url or username
instance HasOptionalParam GetListsActionsByIdList MemberCreatorFields where
  applyOptionalParam req (MemberCreatorFields xs) =
    req `setQuery` toQuery ("memberCreator_fields", Just xs)

-- *** getListsBoardByIdList

-- | @GET \/lists\/{idList}\/board@
-- 
-- getListsBoardByIdList()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
getListsBoardByIdList 
  :: IdList -- ^ "idList" -  idList
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetListsBoardByIdList MimeNoContent NoContent MimeNoContent
getListsBoardByIdList (IdList idList) (Key key) (Token token) =
  _mkRequest "GET" ["/lists/",toPath idList,"/board"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetListsBoardByIdList  

-- | /Optional Param/ "fields" - all or a comma-separated list of: closed, dateLastActivity, dateLastView, desc, descData, idOrganization, invitations, invited, labelNames, memberships, name, pinned, powerUps, prefs, shortLink, shortUrl, starred, subscribed or url
instance HasOptionalParam GetListsBoardByIdList Fields where
  applyOptionalParam req (Fields xs) =
    req `setQuery` toQuery ("fields", Just xs)

-- *** getListsBoardByIdListByField

-- | @GET \/lists\/{idList}\/board\/{field}@
-- 
-- getListsBoardByIdListByField()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
getListsBoardByIdListByField 
  :: IdList -- ^ "idList" -  idList
  -> Field -- ^ "field" -  field
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetListsBoardByIdListByField MimeNoContent NoContent MimeNoContent
getListsBoardByIdListByField (IdList idList) (Field field) (Key key) (Token token) =
  _mkRequest "GET" ["/lists/",toPath idList,"/board/",toPath field]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetListsBoardByIdListByField  

-- *** getListsByIdList

-- | @GET \/lists\/{idList}@
-- 
-- getListsByIdList()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
getListsByIdList 
  :: IdList -- ^ "idList" -  idList
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetListsByIdList MimeNoContent NoContent MimeNoContent
getListsByIdList (IdList idList) (Key key) (Token token) =
  _mkRequest "GET" ["/lists/",toPath idList]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetListsByIdList  

-- | /Optional Param/ "cards" - One of: all, closed, none or open
instance HasOptionalParam GetListsByIdList Cards2 where
  applyOptionalParam req (Cards2 xs) =
    req `setQuery` toQuery ("cards", Just xs)

-- | /Optional Param/ "card_fields" - all or a comma-separated list of: badges, checkItemStates, closed, dateLastActivity, desc, descData, due, email, idAttachmentCover, idBoard, idChecklists, idLabels, idList, idMembers, idMembersVoted, idShort, labels, manualCoverAttachment, name, pos, shortLink, shortUrl, subscribed or url
instance HasOptionalParam GetListsByIdList CardFields where
  applyOptionalParam req (CardFields xs) =
    req `setQuery` toQuery ("card_fields", Just xs)

-- | /Optional Param/ "board" -  true or false
instance HasOptionalParam GetListsByIdList Board where
  applyOptionalParam req (Board xs) =
    req `setQuery` toQuery ("board", Just xs)

-- | /Optional Param/ "board_fields" - all or a comma-separated list of: closed, dateLastActivity, dateLastView, desc, descData, idOrganization, invitations, invited, labelNames, memberships, name, pinned, powerUps, prefs, shortLink, shortUrl, starred, subscribed or url
instance HasOptionalParam GetListsByIdList BoardFields where
  applyOptionalParam req (BoardFields xs) =
    req `setQuery` toQuery ("board_fields", Just xs)

-- | /Optional Param/ "fields" - all or a comma-separated list of: closed, idBoard, name, pos or subscribed
instance HasOptionalParam GetListsByIdList Fields where
  applyOptionalParam req (Fields xs) =
    req `setQuery` toQuery ("fields", Just xs)

-- *** getListsByIdListByField

-- | @GET \/lists\/{idList}\/{field}@
-- 
-- getListsByIdListByField()
-- 
getListsByIdListByField 
  :: IdList -- ^ "idList" -  idList
  -> Field -- ^ "field" -  field
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetListsByIdListByField MimeNoContent NoContent MimeNoContent
getListsByIdListByField (IdList idList) (Field field) (Key key) (Token token) =
  _mkRequest "GET" ["/lists/",toPath idList,"/",toPath field]
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetListsByIdListByField  

-- *** getListsCardsByIdList

-- | @GET \/lists\/{idList}\/cards@
-- 
-- getListsCardsByIdList()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
getListsCardsByIdList 
  :: IdList -- ^ "idList" -  idList
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetListsCardsByIdList MimeNoContent NoContent MimeNoContent
getListsCardsByIdList (IdList idList) (Key key) (Token token) =
  _mkRequest "GET" ["/lists/",toPath idList,"/cards"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetListsCardsByIdList  

-- | /Optional Param/ "actions" - all or a comma-separated list of: addAttachmentToCard, addChecklistToCard, addMemberToBoard, addMemberToCard, addMemberToOrganization, addToOrganizationBoard, commentCard, convertToCardFromCheckItem, copyBoard, copyCard, copyCommentCard, createBoard, createCard, createList, createOrganization, deleteAttachmentFromCard, deleteBoardInvitation, deleteCard, deleteOrganizationInvitation, disablePowerUp, emailCard, enablePowerUp, makeAdminOfBoard, makeNormalMemberOfBoard, makeNormalMemberOfOrganization, makeObserverOfBoard, memberJoinedTrello, moveCardFromBoard, moveCardToBoard, moveListFromBoard, moveListToBoard, removeChecklistFromCard, removeFromOrganizationBoard, removeMemberFromCard, unconfirmedBoardInvitation, unconfirmedOrganizationInvitation, updateBoard, updateCard, updateCard:closed, updateCard:desc, updateCard:idList, updateCard:name, updateCheckItemStateOnCard, updateChecklist, updateList, updateList:closed, updateList:name, updateMember or updateOrganization
instance HasOptionalParam GetListsCardsByIdList Actions2 where
  applyOptionalParam req (Actions2 xs) =
    req `setQuery` toQuery ("actions", Just xs)

-- | /Optional Param/ "attachments" - A boolean value or &quot;cover&quot; for only card cover attachments
instance HasOptionalParam GetListsCardsByIdList Attachments where
  applyOptionalParam req (Attachments xs) =
    req `setQuery` toQuery ("attachments", Just xs)

-- | /Optional Param/ "attachment_fields" - all or a comma-separated list of: bytes, date, edgeColor, idMember, isUpload, mimeType, name, previews or url
instance HasOptionalParam GetListsCardsByIdList AttachmentFields where
  applyOptionalParam req (AttachmentFields xs) =
    req `setQuery` toQuery ("attachment_fields", Just xs)

-- | /Optional Param/ "stickers" -  true or false
instance HasOptionalParam GetListsCardsByIdList Stickers where
  applyOptionalParam req (Stickers xs) =
    req `setQuery` toQuery ("stickers", Just xs)

-- | /Optional Param/ "members" -  true or false
instance HasOptionalParam GetListsCardsByIdList Members2 where
  applyOptionalParam req (Members2 xs) =
    req `setQuery` toQuery ("members", Just xs)

-- | /Optional Param/ "member_fields" - all or a comma-separated list of: avatarHash, bio, bioData, confirmed, fullName, idPremOrgsAdmin, initials, memberType, products, status, url or username
instance HasOptionalParam GetListsCardsByIdList MemberFields where
  applyOptionalParam req (MemberFields xs) =
    req `setQuery` toQuery ("member_fields", Just xs)

-- | /Optional Param/ "checkItemStates" -  true or false
instance HasOptionalParam GetListsCardsByIdList CheckItemStates where
  applyOptionalParam req (CheckItemStates xs) =
    req `setQuery` toQuery ("checkItemStates", Just xs)

-- | /Optional Param/ "checklists" - One of: all or none
instance HasOptionalParam GetListsCardsByIdList Checklists2 where
  applyOptionalParam req (Checklists2 xs) =
    req `setQuery` toQuery ("checklists", Just xs)

-- | /Optional Param/ "limit" - a number from 1 to 1000
instance HasOptionalParam GetListsCardsByIdList Limit where
  applyOptionalParam req (Limit xs) =
    req `setQuery` toQuery ("limit", Just xs)

-- | /Optional Param/ "since" - A date, or null
instance HasOptionalParam GetListsCardsByIdList Since where
  applyOptionalParam req (Since xs) =
    req `setQuery` toQuery ("since", Just xs)

-- | /Optional Param/ "before" - A date, or null
instance HasOptionalParam GetListsCardsByIdList Before where
  applyOptionalParam req (Before xs) =
    req `setQuery` toQuery ("before", Just xs)

-- | /Optional Param/ "filter" - One of: all, closed, none or open
instance HasOptionalParam GetListsCardsByIdList Filter where
  applyOptionalParam req (Filter xs) =
    req `setQuery` toQuery ("filter", Just xs)

-- | /Optional Param/ "fields" - all or a comma-separated list of: badges, checkItemStates, closed, dateLastActivity, desc, descData, due, email, idAttachmentCover, idBoard, idChecklists, idLabels, idList, idMembers, idMembersVoted, idShort, labels, manualCoverAttachment, name, pos, shortLink, shortUrl, subscribed or url
instance HasOptionalParam GetListsCardsByIdList Fields where
  applyOptionalParam req (Fields xs) =
    req `setQuery` toQuery ("fields", Just xs)

-- *** getListsCardsByIdListByFilter

-- | @GET \/lists\/{idList}\/cards\/{filter}@
-- 
-- getListsCardsByIdListByFilter()
-- 
getListsCardsByIdListByFilter 
  :: IdList -- ^ "idList" -  idList
  -> Filter -- ^ "filter" -  filter
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest GetListsCardsByIdListByFilter MimeNoContent NoContent MimeNoContent
getListsCardsByIdListByFilter (IdList idList) (Filter filter) (Key key) (Token token) =
  _mkRequest "GET" ["/lists/",toPath idList,"/cards/",toPath filter]
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data GetListsCardsByIdListByFilter  

-- *** updateListsByIdList

-- | @PUT \/lists\/{idList}@
-- 
-- updateListsByIdList()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
updateListsByIdList 
  :: (Consumes UpdateListsByIdList contentType, MimeRender contentType Lists)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> IdList -- ^ "idList" -  idList
  -> Lists -- ^ "body" -  Attributes of \"Lists\" to be updated.
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest UpdateListsByIdList contentType NoContent MimeNoContent
updateListsByIdList _ (IdList idList) body (Key key) (Token token) =
  _mkRequest "PUT" ["/lists/",toPath idList]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` body
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data UpdateListsByIdList 

-- | /Body Param/ "body" - Attributes of \"Lists\" to be updated.
instance HasBodyParam UpdateListsByIdList Lists 

-- *** updateListsClosedByIdList

-- | @PUT \/lists\/{idList}\/closed@
-- 
-- updateListsClosedByIdList()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
updateListsClosedByIdList 
  :: (Consumes UpdateListsClosedByIdList contentType, MimeRender contentType ListsClosed)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> IdList -- ^ "idList" -  idList
  -> ListsClosed -- ^ "body" -  Attributes of \"Lists Closed\" to be updated.
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest UpdateListsClosedByIdList contentType NoContent MimeNoContent
updateListsClosedByIdList _ (IdList idList) body (Key key) (Token token) =
  _mkRequest "PUT" ["/lists/",toPath idList,"/closed"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` body
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data UpdateListsClosedByIdList 

-- | /Body Param/ "body" - Attributes of \"Lists Closed\" to be updated.
instance HasBodyParam UpdateListsClosedByIdList ListsClosed 

-- *** updateListsIdBoardByIdList

-- | @PUT \/lists\/{idList}\/idBoard@
-- 
-- updateListsIdBoardByIdList()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
updateListsIdBoardByIdList 
  :: (Consumes UpdateListsIdBoardByIdList contentType, MimeRender contentType ListsIdBoard)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> IdList -- ^ "idList" -  idList
  -> ListsIdBoard -- ^ "body" -  Attributes of \"Lists Id Board\" to be updated.
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest UpdateListsIdBoardByIdList contentType NoContent MimeNoContent
updateListsIdBoardByIdList _ (IdList idList) body (Key key) (Token token) =
  _mkRequest "PUT" ["/lists/",toPath idList,"/idBoard"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` body
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data UpdateListsIdBoardByIdList 

-- | /Body Param/ "body" - Attributes of \"Lists Id Board\" to be updated.
instance HasBodyParam UpdateListsIdBoardByIdList ListsIdBoard 

-- *** updateListsNameByIdList

-- | @PUT \/lists\/{idList}\/name@
-- 
-- updateListsNameByIdList()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
updateListsNameByIdList 
  :: (Consumes UpdateListsNameByIdList contentType, MimeRender contentType ListsName)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> IdList -- ^ "idList" -  idList
  -> ListsName -- ^ "body" -  Attributes of \"Lists Name\" to be updated.
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest UpdateListsNameByIdList contentType NoContent MimeNoContent
updateListsNameByIdList _ (IdList idList) body (Key key) (Token token) =
  _mkRequest "PUT" ["/lists/",toPath idList,"/name"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` body
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data UpdateListsNameByIdList 

-- | /Body Param/ "body" - Attributes of \"Lists Name\" to be updated.
instance HasBodyParam UpdateListsNameByIdList ListsName 

-- *** updateListsPosByIdList

-- | @PUT \/lists\/{idList}\/pos@
-- 
-- updateListsPosByIdList()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
updateListsPosByIdList 
  :: (Consumes UpdateListsPosByIdList contentType, MimeRender contentType ListsPos)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> IdList -- ^ "idList" -  idList
  -> ListsPos -- ^ "body" -  Attributes of \"Lists Pos\" to be updated.
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest UpdateListsPosByIdList contentType NoContent MimeNoContent
updateListsPosByIdList _ (IdList idList) body (Key key) (Token token) =
  _mkRequest "PUT" ["/lists/",toPath idList,"/pos"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` body
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data UpdateListsPosByIdList 

-- | /Body Param/ "body" - Attributes of \"Lists Pos\" to be updated.
instance HasBodyParam UpdateListsPosByIdList ListsPos 

-- *** updateListsSubscribedByIdList

-- | @PUT \/lists\/{idList}\/subscribed@
-- 
-- updateListsSubscribedByIdList()
-- 
-- AuthMethod: 'AuthApiKeyApiKey', 'AuthApiKeyApiToken'
-- 
updateListsSubscribedByIdList 
  :: (Consumes UpdateListsSubscribedByIdList contentType, MimeRender contentType ListsSubscribed)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> IdList -- ^ "idList" -  idList
  -> ListsSubscribed -- ^ "body" -  Attributes of \"Lists Subscribed\" to be updated.
  -> Key -- ^ "key" -  <a href=\"https://trello.com/1/appKey/generate\"  target=\"_blank\">Generate your application key</a>
  -> Token -- ^ "token" -  <a href=\"https://trello.com/docs/gettingstarted/index.html#getting-a-token-from-a-user\"  target=\"_blank\">Getting a token from a user</a>
  -> TrelloRequest UpdateListsSubscribedByIdList contentType NoContent MimeNoContent
updateListsSubscribedByIdList _ (IdList idList) body (Key key) (Token token) =
  _mkRequest "PUT" ["/lists/",toPath idList,"/subscribed"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` body
    `setQuery` toQuery ("key", Just key)
    `setQuery` toQuery ("token", Just token)

data UpdateListsSubscribedByIdList 

-- | /Body Param/ "body" - Attributes of \"Lists Subscribed\" to be updated.
instance HasBodyParam UpdateListsSubscribedByIdList ListsSubscribed 