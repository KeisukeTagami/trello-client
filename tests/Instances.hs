{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Instances where

import Trello.Model
import Trello.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (T.pack k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays
    
-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)

-- * Models
 
instance Arbitrary Actions where
  arbitrary =
    Actions
      <$> arbitrary -- actionsText :: Maybe Text
    
instance Arbitrary ActionsComments where
  arbitrary =
    ActionsComments
      <$> arbitrary -- actionsCommentsText :: Maybe Text
    
instance Arbitrary ActionsText where
  arbitrary =
    ActionsText
      <$> arbitrary -- actionsTextValue :: Maybe Text
    
instance Arbitrary Boards where
  arbitrary =
    Boards
      <$> arbitrary -- boardsClosed :: Maybe Text
      <*> arbitrary -- boardsDesc :: Maybe Text
      <*> arbitrary -- boardsIdBoardSource :: Maybe Text
      <*> arbitrary -- boardsIdOrganization :: Maybe Text
      <*> arbitrary -- boardsKeepFromSource :: Maybe Text
      <*> arbitrary -- boardsLabelNamesblue :: Maybe Text
      <*> arbitrary -- boardsLabelNamesgreen :: Maybe Text
      <*> arbitrary -- boardsLabelNamesorange :: Maybe Text
      <*> arbitrary -- boardsLabelNamespurple :: Maybe Text
      <*> arbitrary -- boardsLabelNamesred :: Maybe Text
      <*> arbitrary -- boardsLabelNamesyellow :: Maybe Text
      <*> arbitrary -- boardsName :: Maybe Text
      <*> arbitrary -- boardsPowerUps :: Maybe Text
      <*> arbitrary -- boardsPrefsbackground :: Maybe Text
      <*> arbitrary -- boardsPrefscalendarFeedEnabled :: Maybe Text
      <*> arbitrary -- boardsPrefscardAging :: Maybe Text
      <*> arbitrary -- boardsPrefscardCovers :: Maybe Text
      <*> arbitrary -- boardsPrefscomments :: Maybe Text
      <*> arbitrary -- boardsPrefsinvitations :: Maybe Text
      <*> arbitrary -- boardsPrefspermissionLevel :: Maybe Text
      <*> arbitrary -- boardsPrefsselfJoin :: Maybe Text
      <*> arbitrary -- boardsPrefsvoting :: Maybe Text
      <*> arbitrary -- boardsPrefsBackground :: Maybe Text
      <*> arbitrary -- boardsPrefsCardAging :: Maybe Text
      <*> arbitrary -- boardsPrefsCardCovers :: Maybe Text
      <*> arbitrary -- boardsPrefsComments :: Maybe Text
      <*> arbitrary -- boardsPrefsInvitations :: Maybe Text
      <*> arbitrary -- boardsPrefsPermissionLevel :: Maybe Text
      <*> arbitrary -- boardsPrefsSelfJoin :: Maybe Text
      <*> arbitrary -- boardsPrefsVoting :: Maybe Text
      <*> arbitrary -- boardsSubscribed :: Maybe Text
    
instance Arbitrary BoardsChecklists where
  arbitrary =
    BoardsChecklists
      <$> arbitrary -- boardsChecklistsName :: Maybe Text
    
instance Arbitrary BoardsClosed where
  arbitrary =
    BoardsClosed
      <$> arbitrary -- boardsClosedValue :: Maybe Text
    
instance Arbitrary BoardsDesc where
  arbitrary =
    BoardsDesc
      <$> arbitrary -- boardsDescValue :: Maybe Text
    
instance Arbitrary BoardsIdOrganization where
  arbitrary =
    BoardsIdOrganization
      <$> arbitrary -- boardsIdOrganizationValue :: Maybe Text
    
instance Arbitrary BoardsLabels where
  arbitrary =
    BoardsLabels
      <$> arbitrary -- boardsLabelsColor :: Maybe Text
      <*> arbitrary -- boardsLabelsName :: Maybe Text
    
instance Arbitrary BoardsLists where
  arbitrary =
    BoardsLists
      <$> arbitrary -- boardsListsName :: Maybe Text
      <*> arbitrary -- boardsListsPos :: Maybe Text
    
instance Arbitrary BoardsMembers where
  arbitrary =
    BoardsMembers
      <$> arbitrary -- boardsMembersEmail :: Maybe Text
      <*> arbitrary -- boardsMembersFullName :: Maybe Text
      <*> arbitrary -- boardsMembersType :: Maybe Text
    
instance Arbitrary BoardsMemberships where
  arbitrary =
    BoardsMemberships
      <$> arbitrary -- boardsMembershipsMemberFields :: Maybe Text
      <*> arbitrary -- boardsMembershipsType :: Maybe Text
    
instance Arbitrary BoardsName where
  arbitrary =
    BoardsName
      <$> arbitrary -- boardsNameValue :: Maybe Text
    
instance Arbitrary BoardsPowerUps where
  arbitrary =
    BoardsPowerUps
      <$> arbitrary -- boardsPowerUpsValue :: Maybe Text
    
instance Arbitrary BoardsSubscribed where
  arbitrary =
    BoardsSubscribed
      <$> arbitrary -- boardsSubscribedValue :: Maybe Text
    
instance Arbitrary Cards where
  arbitrary =
    Cards
      <$> arbitrary -- cardsClosed :: Maybe Text
      <*> arbitrary -- cardsDesc :: Maybe Text
      <*> arbitrary -- cardsDue :: Maybe Text
      <*> arbitrary -- cardsFileSource :: Maybe Text
      <*> arbitrary -- cardsIdAttachmentCover :: Maybe Text
      <*> arbitrary -- cardsIdBoard :: Maybe Text
      <*> arbitrary -- cardsIdCardSource :: Maybe Text
      <*> arbitrary -- cardsIdLabels :: Maybe Text
      <*> arbitrary -- cardsIdList :: Maybe Text
      <*> arbitrary -- cardsIdMembers :: Maybe Text
      <*> arbitrary -- cardsKeepFromSource :: Maybe Text
      <*> arbitrary -- cardsLabels :: Maybe Text
      <*> arbitrary -- cardsName :: Maybe Text
      <*> arbitrary -- cardsPos :: Maybe Text
      <*> arbitrary -- cardsSubscribed :: Maybe Text
      <*> arbitrary -- cardsUrlSource :: Maybe Text
    
instance Arbitrary CardsActionsComments where
  arbitrary =
    CardsActionsComments
      <$> arbitrary -- cardsActionsCommentsText :: Maybe Text
    
instance Arbitrary CardsAttachments where
  arbitrary =
    CardsAttachments
      <$> arbitrary -- cardsAttachmentsFile :: Maybe Text
      <*> arbitrary -- cardsAttachmentsMimeType :: Maybe Text
      <*> arbitrary -- cardsAttachmentsName :: Maybe Text
      <*> arbitrary -- cardsAttachmentsUrl :: Maybe Text
    
instance Arbitrary CardsChecklistCheckItem where
  arbitrary =
    CardsChecklistCheckItem
      <$> arbitrary -- cardsChecklistCheckItemName :: Maybe Text
      <*> arbitrary -- cardsChecklistCheckItemPos :: Maybe Text
    
instance Arbitrary CardsChecklistCheckItemName where
  arbitrary =
    CardsChecklistCheckItemName
      <$> arbitrary -- cardsChecklistCheckItemNameValue :: Maybe Text
    
instance Arbitrary CardsChecklistCheckItemPos where
  arbitrary =
    CardsChecklistCheckItemPos
      <$> arbitrary -- cardsChecklistCheckItemPosValue :: Maybe Text
    
instance Arbitrary CardsChecklistCheckItemState where
  arbitrary =
    CardsChecklistCheckItemState
      <$> arbitrary -- cardsChecklistCheckItemStateValue :: Maybe Text
    
instance Arbitrary CardsChecklistIdChecklistCurrentCheckItem where
  arbitrary =
    CardsChecklistIdChecklistCurrentCheckItem
      <$> arbitrary -- cardsChecklistIdChecklistCurrentCheckItemIdChecklist :: Maybe Text
      <*> arbitrary -- cardsChecklistIdChecklistCurrentCheckItemName :: Maybe Text
      <*> arbitrary -- cardsChecklistIdChecklistCurrentCheckItemPos :: Maybe Text
      <*> arbitrary -- cardsChecklistIdChecklistCurrentCheckItemState :: Maybe Text
    
instance Arbitrary CardsChecklists where
  arbitrary =
    CardsChecklists
      <$> arbitrary -- cardsChecklistsIdChecklistSource :: Maybe Text
      <*> arbitrary -- cardsChecklistsName :: Maybe Text
      <*> arbitrary -- cardsChecklistsValue :: Maybe Text
    
instance Arbitrary CardsClosed where
  arbitrary =
    CardsClosed
      <$> arbitrary -- cardsClosedValue :: Maybe Text
    
instance Arbitrary CardsDesc where
  arbitrary =
    CardsDesc
      <$> arbitrary -- cardsDescValue :: Maybe Text
    
instance Arbitrary CardsDue where
  arbitrary =
    CardsDue
      <$> arbitrary -- cardsDueValue :: Maybe Text
    
instance Arbitrary CardsIdAttachmentCover where
  arbitrary =
    CardsIdAttachmentCover
      <$> arbitrary -- cardsIdAttachmentCoverValue :: Maybe Text
    
instance Arbitrary CardsIdBoard where
  arbitrary =
    CardsIdBoard
      <$> arbitrary -- cardsIdBoardIdList :: Maybe Text
      <*> arbitrary -- cardsIdBoardValue :: Maybe Text
    
instance Arbitrary CardsIdLabels where
  arbitrary =
    CardsIdLabels
      <$> arbitrary -- cardsIdLabelsValue :: Maybe Text
    
instance Arbitrary CardsIdList where
  arbitrary =
    CardsIdList
      <$> arbitrary -- cardsIdListValue :: Maybe Text
    
instance Arbitrary CardsIdMembers where
  arbitrary =
    CardsIdMembers
      <$> arbitrary -- cardsIdMembersValue :: Maybe Text
    
instance Arbitrary CardsLabels where
  arbitrary =
    CardsLabels
      <$> arbitrary -- cardsLabelsColor :: Maybe Text
      <*> arbitrary -- cardsLabelsName :: Maybe Text
      <*> arbitrary -- cardsLabelsValue :: Maybe Text
    
instance Arbitrary CardsMembersVoted where
  arbitrary =
    CardsMembersVoted
      <$> arbitrary -- cardsMembersVotedValue :: Maybe Text
    
instance Arbitrary CardsName where
  arbitrary =
    CardsName
      <$> arbitrary -- cardsNameValue :: Maybe Text
    
instance Arbitrary CardsPos where
  arbitrary =
    CardsPos
      <$> arbitrary -- cardsPosValue :: Maybe Text
    
instance Arbitrary CardsStickers where
  arbitrary =
    CardsStickers
      <$> arbitrary -- cardsStickersImage :: Maybe Text
      <*> arbitrary -- cardsStickersLeft :: Maybe Text
      <*> arbitrary -- cardsStickersRotate :: Maybe Text
      <*> arbitrary -- cardsStickersTop :: Maybe Text
      <*> arbitrary -- cardsStickersZIndex :: Maybe Text
    
instance Arbitrary CardsSubscribed where
  arbitrary =
    CardsSubscribed
      <$> arbitrary -- cardsSubscribedValue :: Maybe Text
    
instance Arbitrary Checklists where
  arbitrary =
    Checklists
      <$> arbitrary -- checklistsIdBoard :: Maybe Text
      <*> arbitrary -- checklistsIdCard :: Maybe Text
      <*> arbitrary -- checklistsIdChecklistSource :: Maybe Text
      <*> arbitrary -- checklistsName :: Maybe Text
      <*> arbitrary -- checklistsPos :: Maybe Text
    
instance Arbitrary ChecklistsCheckItems where
  arbitrary =
    ChecklistsCheckItems
      <$> arbitrary -- checklistsCheckItemsChecked :: Maybe Text
      <*> arbitrary -- checklistsCheckItemsName :: Maybe Text
      <*> arbitrary -- checklistsCheckItemsPos :: Maybe Text
    
instance Arbitrary ChecklistsIdCard where
  arbitrary =
    ChecklistsIdCard
      <$> arbitrary -- checklistsIdCardValue :: Maybe Text
    
instance Arbitrary ChecklistsName where
  arbitrary =
    ChecklistsName
      <$> arbitrary -- checklistsNameValue :: Maybe Text
    
instance Arbitrary ChecklistsPos where
  arbitrary =
    ChecklistsPos
      <$> arbitrary -- checklistsPosValue :: Maybe Text
    
instance Arbitrary LabelNamesBlue where
  arbitrary =
    LabelNamesBlue
      <$> arbitrary -- labelNamesBlueValue :: Maybe Text
    
instance Arbitrary LabelNamesGreen where
  arbitrary =
    LabelNamesGreen
      <$> arbitrary -- labelNamesGreenValue :: Maybe Text
    
instance Arbitrary LabelNamesOrange where
  arbitrary =
    LabelNamesOrange
      <$> arbitrary -- labelNamesOrangeValue :: Maybe Text
    
instance Arbitrary LabelNamesPurple where
  arbitrary =
    LabelNamesPurple
      <$> arbitrary -- labelNamesPurpleValue :: Maybe Text
    
instance Arbitrary LabelNamesRed where
  arbitrary =
    LabelNamesRed
      <$> arbitrary -- labelNamesRedValue :: Maybe Text
    
instance Arbitrary LabelNamesYellow where
  arbitrary =
    LabelNamesYellow
      <$> arbitrary -- labelNamesYellowValue :: Maybe Text
    
instance Arbitrary Labels where
  arbitrary =
    Labels
      <$> arbitrary -- labelsColor :: Maybe Text
      <*> arbitrary -- labelsIdBoard :: Maybe Text
      <*> arbitrary -- labelsName :: Maybe Text
    
instance Arbitrary LabelsColor where
  arbitrary =
    LabelsColor
      <$> arbitrary -- labelsColorValue :: Maybe Text
    
instance Arbitrary LabelsName where
  arbitrary =
    LabelsName
      <$> arbitrary -- labelsNameValue :: Maybe Text
    
instance Arbitrary Lists where
  arbitrary =
    Lists
      <$> arbitrary -- listsClosed :: Maybe Text
      <*> arbitrary -- listsIdBoard :: Maybe Text
      <*> arbitrary -- listsIdListSource :: Maybe Text
      <*> arbitrary -- listsName :: Maybe Text
      <*> arbitrary -- listsPos :: Maybe Text
      <*> arbitrary -- listsSubscribed :: Maybe Text
    
instance Arbitrary ListsCards where
  arbitrary =
    ListsCards
      <$> arbitrary -- listsCardsDesc :: Maybe Text
      <*> arbitrary -- listsCardsDue :: Maybe Text
      <*> arbitrary -- listsCardsIdMembers :: Maybe Text
      <*> arbitrary -- listsCardsLabels :: Maybe Text
      <*> arbitrary -- listsCardsName :: Maybe Text
    
instance Arbitrary ListsClosed where
  arbitrary =
    ListsClosed
      <$> arbitrary -- listsClosedValue :: Maybe Text
    
instance Arbitrary ListsIdBoard where
  arbitrary =
    ListsIdBoard
      <$> arbitrary -- listsIdBoardPos :: Maybe Text
      <*> arbitrary -- listsIdBoardValue :: Maybe Text
    
instance Arbitrary ListsMoveAllCards where
  arbitrary =
    ListsMoveAllCards
      <$> arbitrary -- listsMoveAllCardsIdBoard :: Maybe Text
    
instance Arbitrary ListsName where
  arbitrary =
    ListsName
      <$> arbitrary -- listsNameValue :: Maybe Text
    
instance Arbitrary ListsPos where
  arbitrary =
    ListsPos
      <$> arbitrary -- listsPosValue :: Maybe Text
    
instance Arbitrary ListsSubscribed where
  arbitrary =
    ListsSubscribed
      <$> arbitrary -- listsSubscribedValue :: Maybe Text
    
instance Arbitrary Members where
  arbitrary =
    Members
      <$> arbitrary -- membersAvatarSource :: Maybe Text
      <*> arbitrary -- membersBio :: Maybe Text
      <*> arbitrary -- membersFullName :: Maybe Text
      <*> arbitrary -- membersInitials :: Maybe Text
      <*> arbitrary -- membersPrefscolorBlind :: Maybe Text
      <*> arbitrary -- membersPrefslocale :: Maybe Text
      <*> arbitrary -- membersPrefsminutesBetweenSummaries :: Maybe Text
      <*> arbitrary -- membersUsername :: Maybe Text
    
instance Arbitrary MembersAvatar where
  arbitrary =
    MembersAvatar
      <$> arbitrary -- membersAvatarFile :: Maybe Text
    
instance Arbitrary MembersAvatarSource where
  arbitrary =
    MembersAvatarSource
      <$> arbitrary -- membersAvatarSourceValue :: Maybe Text
    
instance Arbitrary MembersBio where
  arbitrary =
    MembersBio
      <$> arbitrary -- membersBioValue :: Maybe Text
    
instance Arbitrary MembersBoardBackgrounds where
  arbitrary =
    MembersBoardBackgrounds
      <$> arbitrary -- membersBoardBackgroundsBrightness :: Maybe Text
      <*> arbitrary -- membersBoardBackgroundsFile :: Maybe Text
      <*> arbitrary -- membersBoardBackgroundsTile :: Maybe Text
    
instance Arbitrary MembersBoardStars where
  arbitrary =
    MembersBoardStars
      <$> arbitrary -- membersBoardStarsIdBoard :: Maybe Text
      <*> arbitrary -- membersBoardStarsPos :: Maybe Text
    
instance Arbitrary MembersBoardStarsIdBoard where
  arbitrary =
    MembersBoardStarsIdBoard
      <$> arbitrary -- membersBoardStarsIdBoardValue :: Maybe Text
    
instance Arbitrary MembersBoardStarsPos where
  arbitrary =
    MembersBoardStarsPos
      <$> arbitrary -- membersBoardStarsPosValue :: Maybe Text
    
instance Arbitrary MembersCustomBoardBackgrounds where
  arbitrary =
    MembersCustomBoardBackgrounds
      <$> arbitrary -- membersCustomBoardBackgroundsBrightness :: Maybe Text
      <*> arbitrary -- membersCustomBoardBackgroundsFile :: Maybe Text
      <*> arbitrary -- membersCustomBoardBackgroundsTile :: Maybe Text
    
instance Arbitrary MembersCustomEmoji where
  arbitrary =
    MembersCustomEmoji
      <$> arbitrary -- membersCustomEmojiFile :: Maybe Text
      <*> arbitrary -- membersCustomEmojiName :: Maybe Text
    
instance Arbitrary MembersCustomStickers where
  arbitrary =
    MembersCustomStickers
      <$> arbitrary -- membersCustomStickersFile :: Maybe Text
    
instance Arbitrary MembersFullName where
  arbitrary =
    MembersFullName
      <$> arbitrary -- membersFullNameValue :: Maybe Text
    
instance Arbitrary MembersInitials where
  arbitrary =
    MembersInitials
      <$> arbitrary -- membersInitialsValue :: Maybe Text
    
instance Arbitrary MembersOneTimeMessagesDismissed where
  arbitrary =
    MembersOneTimeMessagesDismissed
      <$> arbitrary -- membersOneTimeMessagesDismissedValue :: Maybe Text
    
instance Arbitrary MembersSavedSearches where
  arbitrary =
    MembersSavedSearches
      <$> arbitrary -- membersSavedSearchesName :: Maybe Text
      <*> arbitrary -- membersSavedSearchesPos :: Maybe Text
      <*> arbitrary -- membersSavedSearchesQuery :: Maybe Text
    
instance Arbitrary MembersSavedSearchesName where
  arbitrary =
    MembersSavedSearchesName
      <$> arbitrary -- membersSavedSearchesNameValue :: Maybe Text
    
instance Arbitrary MembersSavedSearchesPos where
  arbitrary =
    MembersSavedSearchesPos
      <$> arbitrary -- membersSavedSearchesPosValue :: Maybe Text
    
instance Arbitrary MembersSavedSearchesQuery where
  arbitrary =
    MembersSavedSearchesQuery
      <$> arbitrary -- membersSavedSearchesQueryValue :: Maybe Text
    
instance Arbitrary MembersUsername where
  arbitrary =
    MembersUsername
      <$> arbitrary -- membersUsernameValue :: Maybe Text
    
instance Arbitrary MyPrefsEmailPosition where
  arbitrary =
    MyPrefsEmailPosition
      <$> arbitrary -- myPrefsEmailPositionValue :: Maybe Text
    
instance Arbitrary MyPrefsIdEmailList where
  arbitrary =
    MyPrefsIdEmailList
      <$> arbitrary -- myPrefsIdEmailListValue :: Maybe Text
    
instance Arbitrary MyPrefsShowListGuide where
  arbitrary =
    MyPrefsShowListGuide
      <$> arbitrary -- myPrefsShowListGuideValue :: Maybe Text
    
instance Arbitrary MyPrefsShowSidebar where
  arbitrary =
    MyPrefsShowSidebar
      <$> arbitrary -- myPrefsShowSidebarValue :: Maybe Text
    
instance Arbitrary MyPrefsShowSidebarActivity where
  arbitrary =
    MyPrefsShowSidebarActivity
      <$> arbitrary -- myPrefsShowSidebarActivityValue :: Maybe Text
    
instance Arbitrary MyPrefsShowSidebarBoardActions where
  arbitrary =
    MyPrefsShowSidebarBoardActions
      <$> arbitrary -- myPrefsShowSidebarBoardActionsValue :: Maybe Text
    
instance Arbitrary MyPrefsShowSidebarMembers where
  arbitrary =
    MyPrefsShowSidebarMembers
      <$> arbitrary -- myPrefsShowSidebarMembersValue :: Maybe Text
    
instance Arbitrary Notifications where
  arbitrary =
    Notifications
      <$> arbitrary -- notificationsUnread :: Maybe Text
    
instance Arbitrary NotificationsUnread where
  arbitrary =
    NotificationsUnread
      <$> arbitrary -- notificationsUnreadValue :: Maybe Text
    
instance Arbitrary Organizations where
  arbitrary =
    Organizations
      <$> arbitrary -- organizationsDesc :: Maybe Text
      <*> arbitrary -- organizationsDisplayName :: Maybe Text
      <*> arbitrary -- organizationsName :: Maybe Text
      <*> arbitrary -- organizationsPrefsassociatedDomain :: Maybe Text
      <*> arbitrary -- organizationsPrefsboardVisibilityRestrictorg :: Maybe Text
      <*> arbitrary -- organizationsPrefsboardVisibilityRestrictprivate :: Maybe Text
      <*> arbitrary -- organizationsPrefsboardVisibilityRestrictpublic :: Maybe Text
      <*> arbitrary -- organizationsPrefsexternalMembersDisabled :: Maybe Text
      <*> arbitrary -- organizationsPrefsgoogleAppsVersion :: Maybe Text
      <*> arbitrary -- organizationsPrefsorgInviteRestrict :: Maybe Text
      <*> arbitrary -- organizationsPrefspermissionLevel :: Maybe Text
      <*> arbitrary -- organizationsWebsite :: Maybe Text
    
instance Arbitrary OrganizationsDesc where
  arbitrary =
    OrganizationsDesc
      <$> arbitrary -- organizationsDescValue :: Maybe Text
    
instance Arbitrary OrganizationsDisplayName where
  arbitrary =
    OrganizationsDisplayName
      <$> arbitrary -- organizationsDisplayNameValue :: Maybe Text
    
instance Arbitrary OrganizationsLogo where
  arbitrary =
    OrganizationsLogo
      <$> arbitrary -- organizationsLogoFile :: Maybe Text
    
instance Arbitrary OrganizationsMembers where
  arbitrary =
    OrganizationsMembers
      <$> arbitrary -- organizationsMembersEmail :: Maybe Text
      <*> arbitrary -- organizationsMembersFullName :: Maybe Text
      <*> arbitrary -- organizationsMembersType :: Maybe Text
    
instance Arbitrary OrganizationsMembersDeactivated where
  arbitrary =
    OrganizationsMembersDeactivated
      <$> arbitrary -- organizationsMembersDeactivatedValue :: Maybe Text
    
instance Arbitrary OrganizationsMemberships where
  arbitrary =
    OrganizationsMemberships
      <$> arbitrary -- organizationsMembershipsMemberFields :: Maybe Text
      <*> arbitrary -- organizationsMembershipsType :: Maybe Text
    
instance Arbitrary OrganizationsName where
  arbitrary =
    OrganizationsName
      <$> arbitrary -- organizationsNameValue :: Maybe Text
    
instance Arbitrary OrganizationsWebsite where
  arbitrary =
    OrganizationsWebsite
      <$> arbitrary -- organizationsWebsiteValue :: Maybe Text
    
instance Arbitrary PrefsAssociatedDomain where
  arbitrary =
    PrefsAssociatedDomain
      <$> arbitrary -- prefsAssociatedDomainValue :: Maybe Text
    
instance Arbitrary PrefsBackground where
  arbitrary =
    PrefsBackground
      <$> arbitrary -- prefsBackgroundValue :: Maybe Text
    
instance Arbitrary PrefsBoardVisibilityRestrict where
  arbitrary =
    PrefsBoardVisibilityRestrict
      <$> arbitrary -- prefsBoardVisibilityRestrictValue :: Maybe Text
    
instance Arbitrary PrefsCalendarFeedEnabled where
  arbitrary =
    PrefsCalendarFeedEnabled
      <$> arbitrary -- prefsCalendarFeedEnabledValue :: Maybe Text
    
instance Arbitrary PrefsCardAging where
  arbitrary =
    PrefsCardAging
      <$> arbitrary -- prefsCardAgingValue :: Maybe Text
    
instance Arbitrary PrefsCardCovers where
  arbitrary =
    PrefsCardCovers
      <$> arbitrary -- prefsCardCoversValue :: Maybe Text
    
instance Arbitrary PrefsColorBlind where
  arbitrary =
    PrefsColorBlind
      <$> arbitrary -- prefsColorBlindValue :: Maybe Text
    
instance Arbitrary PrefsComments where
  arbitrary =
    PrefsComments
      <$> arbitrary -- prefsCommentsValue :: Maybe Text
    
instance Arbitrary PrefsExternalMembersDisabled where
  arbitrary =
    PrefsExternalMembersDisabled
      <$> arbitrary -- prefsExternalMembersDisabledValue :: Maybe Text
    
instance Arbitrary PrefsGoogleAppsVersion where
  arbitrary =
    PrefsGoogleAppsVersion
      <$> arbitrary -- prefsGoogleAppsVersionValue :: Maybe Text
    
instance Arbitrary PrefsInvitations where
  arbitrary =
    PrefsInvitations
      <$> arbitrary -- prefsInvitationsValue :: Maybe Text
    
instance Arbitrary PrefsLocale where
  arbitrary =
    PrefsLocale
      <$> arbitrary -- prefsLocaleValue :: Maybe Text
    
instance Arbitrary PrefsMinutesBetweenSummaries where
  arbitrary =
    PrefsMinutesBetweenSummaries
      <$> arbitrary -- prefsMinutesBetweenSummariesValue :: Maybe Text
    
instance Arbitrary PrefsOrgInviteRestrict where
  arbitrary =
    PrefsOrgInviteRestrict
      <$> arbitrary -- prefsOrgInviteRestrictValue :: Maybe Text
    
instance Arbitrary PrefsPermissionLevel where
  arbitrary =
    PrefsPermissionLevel
      <$> arbitrary -- prefsPermissionLevelValue :: Maybe Text
    
instance Arbitrary PrefsSelfJoin where
  arbitrary =
    PrefsSelfJoin
      <$> arbitrary -- prefsSelfJoinValue :: Maybe Text
    
instance Arbitrary PrefsVoting where
  arbitrary =
    PrefsVoting
      <$> arbitrary -- prefsVotingValue :: Maybe Text
    
instance Arbitrary Sessions where
  arbitrary =
    Sessions
      <$> arbitrary -- sessionsIdBoard :: Maybe Text
      <*> arbitrary -- sessionsStatus :: Maybe Text
    
instance Arbitrary SessionsStatus where
  arbitrary =
    SessionsStatus
      <$> arbitrary -- sessionsStatusValue :: Maybe Text
    
instance Arbitrary TokensWebhooks where
  arbitrary =
    TokensWebhooks
      <$> arbitrary -- tokensWebhooksCallbackUrl :: Maybe Text
      <*> arbitrary -- tokensWebhooksDescription :: Maybe Text
      <*> arbitrary -- tokensWebhooksIdModel :: Maybe Text
    
instance Arbitrary Webhooks where
  arbitrary =
    Webhooks
      <$> arbitrary -- webhooksActive :: Maybe Text
      <*> arbitrary -- webhooksCallbackUrl :: Maybe Text
      <*> arbitrary -- webhooksDescription :: Maybe Text
      <*> arbitrary -- webhooksIdModel :: Maybe Text
    
instance Arbitrary WebhooksActive where
  arbitrary =
    WebhooksActive
      <$> arbitrary -- webhooksActiveValue :: Maybe Text
    
instance Arbitrary WebhooksCallbackURL where
  arbitrary =
    WebhooksCallbackURL
      <$> arbitrary -- webhooksCallbackURLValue :: Maybe Text
    
instance Arbitrary WebhooksDescription where
  arbitrary =
    WebhooksDescription
      <$> arbitrary -- webhooksDescriptionValue :: Maybe Text
    
instance Arbitrary WebhooksIdModel where
  arbitrary =
    WebhooksIdModel
      <$> arbitrary -- webhooksIdModelValue :: Maybe Text
    


