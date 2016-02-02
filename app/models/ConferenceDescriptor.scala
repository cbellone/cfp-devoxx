package models

import java.util.Collections

import play.api.Play
import org.joda.time.{DateTimeZone, DateTime}

/**
 * ConferenceDescriptor.
 * This might be the first file to look at, and to customize.
 * Idea behind this file is to try to collect all configurable parameters for a conference.
 *
 * For labels, please do customize messages and messages.fr
 *
 * @author Frederic Camblor
 */

case class ConferenceUrls(faq: String, registration: String,
                          confWebsite: String, cfpHostname: String
                           )

case class ConferenceTiming(
                             datesI18nKey: String,
                             speakersPassDuration: Integer,
                             preferredDayEnabled: Boolean,
                             firstDayFr: String,
                             firstDayEn: String,
                             datesFr: String,
                             datesEn: String,
                             cfpOpenedOn: DateTime,
                             cfpClosedOn: DateTime,
                             scheduleAnnouncedOn: DateTime
                             )

case class ConferenceSponsor(showSponsorProposalCheckbox: Boolean, sponsorProposalType: ProposalType = ProposalType.UNKNOWN)

case class TrackDesc(id: String, imgSrc: String, i18nTitleProp: String, i18nDescProp: String)

case class ProposalConfiguration(id: String, slotsCount: Int,
                                 givesSpeakerFreeEntrance: Boolean,
                                 freeEntranceDisplayed: Boolean,
                                 htmlClass: String,
                                 hiddenInCombo: Boolean = false,
                                 chosablePreferredDay: Boolean = false,
                                 impliedSelectedTrack: Option[Track] = None)

object ProposalConfiguration {

  val UNKNOWN = ProposalConfiguration(id = "unknown", slotsCount = 0, givesSpeakerFreeEntrance = false, freeEntranceDisplayed = false,
    htmlClass = "", hiddenInCombo = true, chosablePreferredDay = false)

  def parse(propConf: String): ProposalConfiguration = {
    ConferenceDescriptor.ConferenceProposalConfigurations.ALL.find(p => p.id == propConf).getOrElse(ProposalConfiguration.UNKNOWN)
  }

  def totalSlotsCount = ConferenceDescriptor.ConferenceProposalConfigurations.ALL.map(_.slotsCount).sum

  def isDisplayedFreeEntranceProposals(pt: ProposalType): Boolean = {
    ConferenceDescriptor.ConferenceProposalConfigurations.ALL.filter(p => p.id == pt.id).map(_.freeEntranceDisplayed).headOption.getOrElse(false)
  }

  def getProposalsImplyingATrackSelection = {
    ConferenceDescriptor.ConferenceProposalConfigurations.ALL.filter(p => p.impliedSelectedTrack.nonEmpty)
  }

  def getHTMLClassFor(pt: ProposalType): String = {
    ConferenceDescriptor.ConferenceProposalConfigurations.ALL.filter(p => p.id == pt.id).map(_.htmlClass).headOption.getOrElse("unknown")
  }

  def isChosablePreferredDaysProposals(pt: ProposalType): Boolean = {
    ConferenceDescriptor.ConferenceProposalConfigurations.ALL.filter(p => p.id == pt.id).map(_.chosablePreferredDay).headOption.getOrElse(false)
  }

  def doesProposalTypeGiveSpeakerFreeEntrance(pt: ProposalType): Boolean = {
    ConferenceDescriptor.ConferenceProposalConfigurations.ALL.filter(p => p.id == pt.id).map(_.givesSpeakerFreeEntrance).headOption.getOrElse(false)
  }
}

case class ConferenceDescriptor(eventCode: String,
                                confUrlCode: String,
                                itEnabled: Boolean,
                                fromEmail: String,
                                committeeEmail: String,
                                bccEmail: Option[String],
                                bugReportRecipient: String,
                                conferenceUrls: ConferenceUrls,
                                timing: ConferenceTiming,
                                hosterName: String,
                                hosterWebsite: String,
                                hashTag: String,
                                conferenceSponsor: ConferenceSponsor,
                                locale: List[String],
                                localisation: String,
                                showQuestion:Boolean
                                 )

object ConferenceDescriptor {

  object ConferenceProposalTypes {
    val CONF = ProposalType(id = "conf", label = "conf.label")

    val UNI = ProposalType(id = "uni", label = "uni.label")

    val TIA = ProposalType(id = "tia", label = "tia.label")

    val LAB = ProposalType(id = "lab", label = "lab.label")

    val QUICK = ProposalType(id = "quick", label = "quick.label")

    val BOF = ProposalType(id = "bof", label = "bof.label")

    val KEY = ProposalType(id = "key", label = "key.label")

    val HACK = ProposalType(id = "hack", label = "hack.label")

    val CODE = ProposalType(id = "cstory", label = "code.label")

    val AMD = ProposalType(id = "amd", label = "amd.label")

    val OTHER = ProposalType(id = "other", label = "other.label")

    val ALL = List(CONF, QUICK, KEY)

    def valueOf(id: String): ProposalType = id match {
      case "conf" => CONF
      case "uni" => UNI
      case "tia" => TIA
      case "lab" => LAB
      case "quick" => QUICK
      case "bof" => BOF
      case "key" => KEY
      case "hack" => HACK
      case "cstory" => CODE
      case "amd" => AMD
      case "other" => OTHER
    }

  }

  object ConferenceProposalConfigurations {
    val CONF = ProposalConfiguration(id = "conf", slotsCount = 21, givesSpeakerFreeEntrance = true, freeEntranceDisplayed = true, htmlClass = "icon-microphone",
      chosablePreferredDay = true)
    val UNI = ProposalConfiguration(id = "uni", slotsCount = 16, givesSpeakerFreeEntrance = true, freeEntranceDisplayed = true, htmlClass = "icon-laptop",
      chosablePreferredDay = true)
    val TIA = ProposalConfiguration(id = "tia", slotsCount = 24, givesSpeakerFreeEntrance = true, freeEntranceDisplayed = true, htmlClass = "icon-legal",
      chosablePreferredDay = true)
    val LAB = ProposalConfiguration(id = "lab", slotsCount = 10, givesSpeakerFreeEntrance = true, freeEntranceDisplayed = true, htmlClass = "icon-beaker",
      chosablePreferredDay = true)
    val QUICK = ProposalConfiguration(id = "quick", slotsCount = 6, givesSpeakerFreeEntrance = false, freeEntranceDisplayed = false, htmlClass = "icon-fast-forward",
      chosablePreferredDay = true)
    val BOF = ProposalConfiguration(id = "bof", slotsCount = 25, givesSpeakerFreeEntrance = false, freeEntranceDisplayed = false, htmlClass = "icon-group",
      chosablePreferredDay = false)
    val KEY = ProposalConfiguration(id = "key", slotsCount = 1, givesSpeakerFreeEntrance = true, freeEntranceDisplayed = false, htmlClass = "icon-microphone",
      chosablePreferredDay = false)
    val HACK = ProposalConfiguration(id = "hack", slotsCount = 1, givesSpeakerFreeEntrance = false, freeEntranceDisplayed = false, htmlClass = "icon-microphone",
      chosablePreferredDay = false)
    val CODE = ProposalConfiguration(id = "cstory", slotsCount = 1, givesSpeakerFreeEntrance = false, freeEntranceDisplayed = false, htmlClass = "icon-microphone",
      chosablePreferredDay = false)
    val AMD = ProposalConfiguration(id = "amd", slotsCount = 1, givesSpeakerFreeEntrance = false, freeEntranceDisplayed = false, htmlClass = "icon-microphone",
      chosablePreferredDay = false)
    val OTHER = ProposalConfiguration(id = "other", slotsCount = 1, givesSpeakerFreeEntrance = false, freeEntranceDisplayed = false, htmlClass = "icon-microphone",
      hiddenInCombo = true, chosablePreferredDay = false)
    val ALL = List(CONF, QUICK, KEY)

    def doesItGivesSpeakerFreeEntrance(proposalType: ProposalType): Boolean = {
      ALL.filter(_.id == proposalType.id).exists(_.givesSpeakerFreeEntrance)
    }
  }

  object ConferenceTracks {
    val WEB_MOBILE = Track("wm", "webmobile.label")
    val ARCHISEC = Track("archisec", "archisec.label")
    val AGILITY_TESTS = Track("agTest", "agilityTest.label")
    val JAVA = Track("java", "java.label")
    val CLOUDDEVOPS = Track("cldops", "cloudDevops.label")
    val BIGDATA = Track("bigd", "bigdata.label")
    val FUTURE = Track("future", "future.label")
    val LANG = Track("lang", "lang.label")
    val UNKNOWN = Track("unknown", "unknown track")
    val ALL = List(WEB_MOBILE, ARCHISEC, AGILITY_TESTS, JAVA, CLOUDDEVOPS, BIGDATA, FUTURE, LANG, UNKNOWN)
  }

  object ConferenceTracksDescription {
    val WEB_MOBILE = TrackDesc(ConferenceTracks.WEB_MOBILE.id, "/assets/vdt16/images/icon_web.png", "track.webmobile.title", "track.webmobile.desc")
    val ARCHISEC = TrackDesc(ConferenceTracks.ARCHISEC.id, "/assets/vdt16/images/icon_architecture.png", "track.archisec.title", "track.archisec.desc")
    val AGILITY_TESTS = TrackDesc(ConferenceTracks.AGILITY_TESTS.id, "/assets/vdt16/images/icon_startup.png", "track.agilityTest.title", "track.agilityTest.desc")
    val JAVA = TrackDesc(ConferenceTracks.JAVA.id, "/assets/vdt16/images/icon_javase.png", "track.java.title", "track.java.desc")
    val CLOUDDEVOPS = TrackDesc(ConferenceTracks.CLOUDDEVOPS.id, "/assets/vdt16/images/icon_cloud.png", "track.cloudDevops.title", "track.cloudDevops.desc")
    val BIGDATA = TrackDesc(ConferenceTracks.BIGDATA.id, "/assets/vdt16/images/icon_mobile.png", "track.bigdata.title", "track.bigdata.desc")
    val FUTURE = TrackDesc(ConferenceTracks.FUTURE.id, "/assets/vdt16/images/icon_future.png", "track.future.title", "track.future.desc")
    val LANG = TrackDesc(ConferenceTracks.LANG.id, "/assets/vdt16/images/icon_alternative.png", "track.lang.title", "track.lang.desc")
    val ALL = List(WEB_MOBILE, ARCHISEC, AGILITY_TESTS, JAVA, CLOUDDEVOPS, BIGDATA, FUTURE, LANG)

    def findTrackDescFor(t: Track): TrackDesc = {
      ALL.find(_.id == t.id).head
    }
  }

  object ConferenceRooms {

    // Tip : I use the ID to sort-by on the view per day... So if the exhibition floor id is "aaa" it will be
    // the first column on the HTML Table
    val HALL_EXPO = Room("a_hall", "Exhibition floor", 1500, "special", "rien")

    val ROOM3 = Room("room3", "Room 3", 345, "theatre", "camera")
    val ROOM4 = Room("room4", "Room 4", 364, "theatre", "camera")
    val ROOMB1 = Room("roomB1", "Room B1", 684, "theatre", "camera")
    val ROOMC = Room("roomC", "Room C", 407, "theatre", "camera")
    val ROOMB2 = Room("roomB2", "Room B2", 407, "theatre", "camera")
    val ROOMB = Room("roomB", "Room B", 745, "theatre", "camera")
    val ROOMB3 = Room("roomB3", "Room B3", 425, "theatre", "camera")

    val BOF1 = Room("bof1", "BOF 1", 70, "classroom", "camera")
    val BOF2 = Room("bof2", "BOF 2", 70, "classroom", "camera")

    val keynoteRoom = List(ROOMB)

    val allRoomsConf = List(ROOMB1, ROOMB3, ROOMC)

    val allRooms = List(ROOMB, ROOMB1, ROOMB3, ROOMC, ROOMB2, HALL_EXPO)
  }

  object ConferenceSlotBreaks {
    val registration = SlotBreak("reg", "Registration, Welcome and Breakfast", "Accueil", ConferenceRooms.HALL_EXPO)
    val petitDej = SlotBreak("dej", "Breakfast", "Accueil et petit-déjeuner", ConferenceRooms.HALL_EXPO)
    val coffee = SlotBreak("coffee", "Coffee Break", "Pause café", ConferenceRooms.HALL_EXPO)
    val lunch = SlotBreak("lunch", "Lunch", "Pause déjeuner", ConferenceRooms.HALL_EXPO)
    val shortBreak = SlotBreak("chgt", "Break", "Pause courte", ConferenceRooms.HALL_EXPO)
    val exhibition = SlotBreak("exhib", "Exhibition", "Exhibition", ConferenceRooms.HALL_EXPO)
    val meetAndGreet = SlotBreak("meet", "Meet & Greet (Exhibition)", "Exhibition", ConferenceRooms.HALL_EXPO)
    val eveningKeynote = SlotBreak("evKey", "Evening Keynote", "Keynote", ConferenceRooms.ROOMB)
    val closingKeynote = SlotBreak("closeKey", "Closing Keynote", "Keynote", ConferenceRooms.ROOMB)
    val movieSpecial = SlotBreak("movie", "Closing keynote 19:00-19:30 - Movie 20:00-22:00", "Movie", ConferenceRooms.HALL_EXPO)
    val noxx = SlotBreak("noxx", "Noxx party", "Soirée au Noxx", ConferenceRooms.HALL_EXPO)
  }

  object ConferenceSlots {

    val quickiesSlotsSaturday: List[Slot] = {

      val quickiesSaturdayLunch1 = ConferenceRooms.allRoomsConf.map {
        r1 =>
          SlotBuilder(ConferenceProposalTypes.QUICK.id, "saturday", new DateTime("2016-04-30T12:10:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Zurich")), new DateTime("2016-04-30T12:30:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Zurich")), r1)
      }
      val quickiesSaturdayLunch2 = ConferenceRooms.allRoomsConf.map {
        r2 =>
          SlotBuilder(ConferenceProposalTypes.QUICK.id, "saturday", new DateTime("2016-04-30T12:40:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Zurich")), new DateTime("2016-04-30T13:00:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Zurich")), r2)
      }
      quickiesSaturdayLunch1 ++ quickiesSaturdayLunch2
    }

    // CONFERENCE KEYNOTES

    val keynoteSlotssaturday: List[Slot] = {

      val keynoteSaturdaySlot1 = ConferenceRooms.keynoteRoom.map {
        r1 =>
          SlotBuilder(ConferenceProposalTypes.KEY.id, "saturday",
            new DateTime("2016-04-30T09:15:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Zurich")),
            new DateTime("2016-04-30T10:00:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Zurich")), r1)
      }
      keynoteSaturdaySlot1
    }

    // CONFERENCE SLOTS

    val conferenceSlotsSaturday: List[Slot] = {

      val conferenceSaturdaySlot1 = ConferenceRooms.allRoomsConf.map {
        r1 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "saturday", new DateTime("2016-04-30T10:30:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Zurich")), new DateTime("2016-04-30T11:20:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Zurich")), r1)
      }
      val conferenceSaturdaySlot2 = ConferenceRooms.allRoomsConf.map {
        r2 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "saturday", new DateTime("2016-04-30T11:30:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Zurich")), new DateTime("2016-04-30T12:20:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Zurich")), r2)
      }
      val conferenceSaturdaySlot3 = ConferenceRooms.allRoomsConf.map {
        r3 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "saturday", new DateTime("2016-04-30T12:30:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Zurich")), new DateTime("2016-04-30T13:20:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Zurich")), r3)
      }
      val conferenceSaturdaySlot4 = ConferenceRooms.allRoomsConf.map {
        r4 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "saturday", new DateTime("2016-04-30T14:20:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Zurich")), new DateTime("2016-04-30T15:10:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Zurich")), r4)
      }

      val conferenceSaturdaySlot5 = ConferenceRooms.allRoomsConf.map {
        r5 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "saturday", new DateTime("2016-04-30T15:20:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Zurich")), new DateTime("2016-04-30T16:10:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Zurich")), r5)
      }
      val conferenceSaturdaySlot6 = ConferenceRooms.allRoomsConf.map {
        r6 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "saturday", new DateTime("2016-04-30T16:40:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Zurich")), new DateTime("2016-04-30T17:20:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Zurich")), r6)
      }
      val conferenceSaturdaySlot7 = ConferenceRooms.allRoomsConf.map {
        r6 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "saturday", new DateTime("2016-04-30T17:30:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Zurich")), new DateTime("2016-04-30T18:20:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Zurich")), r6)
      }

      conferenceSaturdaySlot1 ++ conferenceSaturdaySlot2 ++ conferenceSaturdaySlot3 ++ conferenceSaturdaySlot4 ++ conferenceSaturdaySlot5 ++ conferenceSaturdaySlot6 ++ conferenceSaturdaySlot7
    }

    val saturdayBreaks = List(
      SlotBuilder(ConferenceSlotBreaks.petitDej, "Saturday", new DateTime("2016-04-30T10:00:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Zurich")), new DateTime("2016-04-30T10:30:00.000+02:00"))
      , SlotBuilder(ConferenceSlotBreaks.lunch, "Saturday", new DateTime("2016-04-30T13:20:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Zurich")), new DateTime("2016-04-30T14:20:00.000+02:00"))
      , SlotBuilder(ConferenceSlotBreaks.coffee, "Saturday", new DateTime("2016-04-30T16:10:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Zurich")), new DateTime("2016-04-30T16:40:00.000+02:00"))
      , SlotBuilder(ConferenceSlotBreaks.meetAndGreet, "Saturday", new DateTime("2016-04-30T18:30:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Zurich")), new DateTime("2016-04-30T19:00:00.000+02:00"))
    )

    val saturday: List[Slot] = {
      saturdayBreaks ++ keynoteSlotssaturday ++ conferenceSlotsSaturday ++ quickiesSlotsSaturday
    }

    val wednesday: List[Slot] = List.empty

    val friday: List[Slot] = List.empty

    // COMPLETE DEVOXX

    def all: List[Slot] = {
      saturday
    }
  }

  def current() = ConferenceDescriptor(
    eventCode = "VDT16",
    // You will need to update conf/routes files with this code if modified
    confUrlCode = "VDT16",
    itEnabled = true,
    fromEmail = Play.current.configuration.getString("mail.from").getOrElse("info@exteso.com"),
    committeeEmail = Play.current.configuration.getString("mail.committee.email").getOrElse("info@exteso.com"),
    bccEmail = Play.current.configuration.getString("mail.bcc"),
    bugReportRecipient = Play.current.configuration.getString("mail.bugreport.recipient").getOrElse("info@exteso.com"),
    conferenceUrls = ConferenceUrls(
      faq = "https://voxxeddays.com/ticino16/",
      registration = "https://tickets.voxxeddays.com/event/vdt16/",
      confWebsite = "https://voxxeddays.com/ticino16/",
      cfpHostname = Play.current.configuration.getString("cfp.hostname").getOrElse("cfp-vdt.exteso.com")
    ),
    timing = ConferenceTiming(
      datesI18nKey = "30th April 2016",
      speakersPassDuration = 1,
      preferredDayEnabled = false,
      firstDayFr = "8 avril",
      firstDayEn = "April 30th",
      datesFr = "du 8 au 10 avril 2015",
      datesEn = "30th of April, 2016",
      cfpOpenedOn = DateTime.parse("2015-11-10T00:00:00+01:00"),
      cfpClosedOn = DateTime.parse("2016-01-25T23:59:59+01:00"),
      scheduleAnnouncedOn = DateTime.parse("2016-03-01T00:00:00+01:00")
    ),
    hosterName = "", hosterWebsite = "",
    hashTag = "#VDT16",
    conferenceSponsor = ConferenceSponsor(showSponsorProposalCheckbox = true, sponsorProposalType = ConferenceProposalTypes.CONF)
    , List("en_GB", "it")
    , "Palazzo dei Congressi, 6900 Lugano"
    ,showQuestion=false
  )

  val isCFPOpen: Boolean = {
    false
    //current().timing.cfpOpenedOn.isBeforeNow && current().timing.cfpClosedOn.isAfterNow
  }

}
