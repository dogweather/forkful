---
title:    "Ruby: Ein Datum in einen String umwandeln"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von einem Datum in einen String ist ein wichtiges Konzept in Ruby, das es uns ermöglicht, Datumsangaben in einem bestimmten Format auszugeben. Es kann hilfreich sein, wenn wir beispielsweise Daten in unserem Programm anzeigen oder speichern möchten.

## Wie man es macht

Um ein Datum in einen String umzuwandeln, können wir die `strftime` Methode verwenden. Diese Methode formatiert das Datum entsprechend dem angegebenen Format und gibt es als String zurück. Hier ist ein Beispiel:

```Ruby
date = Time.now
puts date.strftime("%d.%m.%Y")
```

Dieses Code-Beispiel gibt das aktuelle Datum im Format "Tag.Monat.Jahr" aus. Hier sind einige gängige Formatierungsoptionen:

- `%d` - Tag (z.B. 01, 02, 03)
- `%m` - Monat (z.B. 01, 02, 03)
- `%Y` - Jahr (z.B. 2021)
- `%H` - Stunde im 24-Stunden-Format (z.B. 13, 14, 15)
- `%M` - Minute (z.B. 05, 10, 15)
- `%S` - Sekunde (z.B. 01, 02, 03)

Es gibt noch viele weitere Optionen, die je nach Bedarf angepasst werden können. Eine vollständige Liste der Formatierungsoptionen findest du in der offiziellen Ruby-Dokumentation.

## Tiefergehende Informationen

Das Konvertieren von Datum in einen String kann auch eine Zeitzone angegeben werden, wenn du mit Zeitangaben in verschiedenen Zeitzonen arbeiten möchtest. Dazu kannst du die `strftime` Methode um den Parameter `%z` erweitern.

```Ruby
date = Time.now
puts date.strftime("%d.%m.%Y - %H:%M %z")
```

Dieses Code-Beispiel gibt das aktuelle Datum und die Uhrzeit aus, zusammen mit der Zeitzone (z.B. +0200 für Mitteleuropäische Sommerzeit).

Außerdem können wir auch die `parse` Methode verwenden, um einen String wieder in ein Datum umzuwandeln. Hier ist ein Beispiel:

```Ruby
date_string = "10.05.2021"
date = Time.parse(date_string)
puts date.strftime("%d.%m.%Y")
```

Dieses Code-Beispiel nimmt den String "10.05.2021" und konvertiert ihn wieder in ein Datum, welches dann wieder im gewünschten Format ausgegeben wird.

## Siehe auch

- [Offizielle Ruby-Dokumentation zu `strftime`](https://ruby-doc.org/core-3.0.0/Time.html#method-i-strftime)
- [Liste der Formatierungsoptionen für `strftime`](https://apidock.com/ruby/Time/strftime)
- [Offizielle Ruby-Dokumentation zu `parse`](https://ruby-doc.org/core-3.0.0/Time.html#method-i-parse)