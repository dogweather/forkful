---
title:    "Haskell: Umwandeln eines Datums in eine Zeichenkette"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren eines Datums in einen String kann sehr nützlich sein, wenn man zum Beispiel eine Benutzeroberfläche hat, die das Datum in einer Textform anzeigen muss. Auch für die Arbeit mit Datenbanken oder dem Schreiben von Dateien kann es hilfreich sein, das Datum in einem lesbaren Format zu haben.

## Wie

Um ein Datum in einen String umzuwandeln, können wir die `formatTime` Funktion aus dem `Data.Time` Modul verwenden. Diese Funktion hat zwei Parameter: ein Datum und ein Formatierungstext, der angibt, wie das Datum ausgegeben werden soll.

Hier ist ein Beispiel-Code, der ein Datum in einen String mit dem Format "Tag-Monat-Jahr" konvertiert:

```Haskell
import Data.Time

-- Datum erstellen
let date = fromGregorian 2021 4 15

-- Datum in String umwandeln
let dateString = formatTime defaultTimeLocale "%d-%m-%Y" date

-- Ausgabe
print dateString
-- Ergebnis: "15-04-2021"
```

In diesem Beispiel verwenden wir die Standardzeitzone und das englische Datumsformat. Der Formatierungstext kann jedoch an persönliche Vorlieben und Anforderungen angepasst werden. Eine vollständige Liste der verfügbaren Formatierungsoptionen finden Sie in der offiziellen [Haskell-Dokumentation](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html).

## Deep Dive

Die `formatTime` Funktion verwendet das `FormatTime` Typklasse, um ein Datum in einen String umzuwandeln. Diese Typklasse definiert die `formatTime` Methode, die dann von konkreten Datentypen wie `UTCTime`, `ZonedTime` oder `LocalTime` implementiert wird.

Wenn Sie mehr über Typklassen und ihre Verwendung erfahren möchten, empfehlen wir die Lektüre des Artikels [Haskell Typklassen erklärt](https://coderscat.com/haskell-typ-klasse/). Dort erfahren Sie, wie Typklassen in Haskell funktionieren und wie Sie sie in Ihrer eigenen Codebasis verwenden können.

## Siehe auch

- [Data.Time Dokumentation](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [FormatTime Typklasse Dokumentation](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html)
- [Haskell Typklassen erklärt](https://coderscat.com/haskell-typ-klasse/)