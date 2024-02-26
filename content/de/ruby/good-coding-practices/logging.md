---
date: 2024-01-26 01:08:27.465647-07:00
description: "Das Protokollieren in der Programmierung gleicht dem F\xFChren eines\
  \ Tagebuchs f\xFCr Ihre Anwendung. Es ist die systematische Aufzeichnung von Ereignissen,\u2026"
lastmod: '2024-02-25T18:49:51.453403-07:00'
model: gpt-4-1106-preview
summary: "Das Protokollieren in der Programmierung gleicht dem F\xFChren eines Tagebuchs\
  \ f\xFCr Ihre Anwendung. Es ist die systematische Aufzeichnung von Ereignissen,\u2026"
title: Protokollierung
---

{{< edit_this_page >}}

## Was & Warum?
Das Protokollieren in der Programmierung gleicht dem Führen eines Tagebuchs für Ihre Anwendung. Es ist die systematische Aufzeichnung von Ereignissen, Nachrichten und Datenpunkten, die Ihnen Einblick geben in das, was Ihre Anwendung tut und wie sie sich verhält. Entwickler protokollieren, weil es entscheidend für das Debugging ist, die Überwachung der Anwendungsgesundheit und das Erhalten von Hinweisen über potenzielle Probleme, bevor diese zu echten Schwierigkeiten werden.

## Wie man das macht:
Ruby verfügt über ein eingebautes Modul zum Protokollieren, `Logger`, das super einfach zu verwenden ist. Hier ist ein schnelles Beispiel, um Ihnen den Einstieg zu erleichtern:

```ruby
require 'logger'

# Erstellen Sie einen Logger, der an STDOUT ausgibt
logger = Logger.new(STDOUT)
logger.level = Logger::INFO

# Beispiel-Lognachrichten
logger.info("Dies ist eine Info-Nachricht")
logger.warn("Dies ist eine Warnmeldungen")
logger.error("Dies ist eine Fehlermeldung")
```

Die Ausführung des obigen Skripts erzeugt eine Ausgabe wie diese:

```
I, [2023-03-15T10:00:00.123456 #1234]  INFO -- : Dies ist eine Info-Nachricht
W, [2023-03-15T10:00:01.234567 #1234]  WARN -- : Dies ist eine Warnmeldung
E, [2023-03-15T10:00:02.345678 #1234] ERROR -- : Dies ist eine Fehlermeldung
```

Sie können das Log-Format und das Level konfigurieren, um unnötigen Lärm herauszufiltern, und Sie können die Protokolle auf unterschiedliche Ausgaben umleiten, wie eine Datei oder sogar einen externen Protokollierdienst.

## Vertiefung
Das Protokollieren ist wie eine uralte Tradition in der Programmierung. Historisch waren Logs einfache Textdateien, die manuell mit Tools wie `grep` analysiert wurden. Aber das Konzept entwickelte sich zu einem ganzen Ökosystem von robusten Protokollierungs-Frameworks und -Diensten wie Log4j, Syslog auf Linux oder Sematext und Loggly im Cloud-Zeitalter.

Rubys `Logger` ist eine unkomplizierte Möglichkeit, zu beginnen, aber wenn Sie mehr Leistung und Flexibilität benötigen, könnten Sie Alternativen wie Lograge oder Semantic Logger in Betracht ziehen. Diese Bibliotheken arbeiten gut mit Ruby-Anwendungen zusammen und bieten eine feiner abgestimmte Kontrolle über das Log-Format, einschließlich strukturierter Logs (im JSON-Format), eine bessere Leistung und eine nahtlose Integration mit anderen Diensten.

Jede Ruby-Logging-Bibliothek hat ihre eigene Art, Dinge zu erledigen, aber hinter den Kulissen drehen sie alle um die Idee einer Logger-Instanz, an die Sie Nachrichten senden. Der Logger verarbeitet diese Nachrichten basierend auf festgelegten Leveln—DEBUG, INFO, WARN, ERROR, FATAL und UNKNOWN—und entscheidet, was mit ihnen zu tun ist: Sie ausdrucken, in eine Datei speichern, über das Netzwerk senden usw.

## Siehe auch
Für einen tieferen Einblick in das eingebaute Logging-Modul von Ruby, schauen Sie in die offizielle Dokumentation:

Wenn Sie an fortgeschrittenerem Logging interessiert sind oder Drittanbieter-Gems erkunden möchten:
- [Lograge](https://github.com/roidrage/lograge)

Für allgemeine Protokollführungspraktiken und -philosophie (nicht Ruby-spezifisch), sind diese Artikel zeitlose Lektüren:
- [Googles Site Reliability Engineering Buch - Kapitel 16: Überlastung bewältigen](https://sre.google/sre-book/handling-overload/#log-messages)
- [Die 12-Faktor-App - Logs](https://12factor.net/logs)
