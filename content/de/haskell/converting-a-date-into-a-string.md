---
title:    "Haskell: Eine Datum in einen String umwandeln"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Warum

Als Programmierer kann es oft nützlich sein, ein Datum in einen String umzuwandeln. Möglicherweise müssen Sie es in einem bestimmten Format anzeigen oder an eine externe API senden, die nur Strings akzeptiert. In diesem Blog-Artikel werden wir uns ansehen, wie man ein Datum in Haskell in eine Zeichenfolge konvertieren kann.

## Wie geht das?

Um ein Datum in Haskell in einen String umzuwandeln, können wir die Funktion `show` verwenden. Diese Funktion akzeptiert ein Datum als Eingabe und gibt einen String zurück, der das Datum repräsentiert. Schauen wir uns ein Beispiel an:

```Haskell
import Data.Time.Calendar

myDate :: Day
myDate = fromGregorian 2021 10 10

main :: IO ()
main = do
    let dateString = show myDate
    putStrLn dateString
```

Das obige Beispiel importiert das `Data.Time.Calendar` Modul, um den Datentyp `Day` zu verwenden. Dann erstellen wir ein Datum mit dem Jahr 2021, dem Monat 10 und dem Tag 10. Anschließend verwenden wir `show` und geben unserem Datum als Argument, um den String mit dem Datum zu erhalten. Die Ausgabe wird `2021-10-10` sein.

Es ist auch möglich, das Datum in einem bestimmten Format anzuzeigen, indem wir `formatTime` verwenden. Diese Funktion erfordert die Angabe eines Formats sowie des Datums. Schauen wir uns ein weiteres Beispiel an:

```Haskell
import Data.Time.Format

myDate :: Day
myDate = fromGregorian 2021 10 10

main :: IO ()
main = do
    let dateString = formatTime defaultTimeLocale "%d.%m.%Y" myDate
    putStrLn dateString
```

In diesem Beispiel verwenden wir `formatTime` und geben als erstes Argument `defaultTimeLocale` an, um das Standardformat zu verwenden. Das zweite Argument ist das gewünschte Format, in diesem Fall den Tag, den Monat und das Jahr getrennt durch Punkte. Die Ausgabe wird `10.10.2021` sein.

## Tiefergehende Informationen

Haskell hat verschiedene Datentypen, die verwendet werden können, um ein Datum zu repräsentieren. Neben dem oben verwendeten `Day` gibt es auch `LocalTime`, `ZonedDateTime` und mehr. Jeder dieser Datentypen hat seine eigenen Funktionen, um ein Datum in eine Zeichenfolge umzuwandeln. Daher ist es wichtig, die Dokumentation des jeweiligen Datentyps zu überprüfen, um die beste Option für Ihre Bedürfnisse zu finden.

Es gibt auch verschiedene Möglichkeiten, ein Datum mit `formatTime` zu formatieren, wie zum Beispiel die Verwendung von Wochentagen oder Uhrzeiten. Weitere Informationen zu den unterstützten Formaten finden Sie in der offiziellen Dokumentation von Haskell.

## Siehe auch

- Die offizielle [Haskell-Dokumentation zu Datum und Zeit](https://hackage.haskell.org/package/time/docs/Data-Time-Calendar.html)
- Ein Tutorial für Daten und Zeit in Haskell von Learn You a Haskell for Great Good [auf Deutsch](https://learnyouahaskell.wordpress.com/2010/09/09/datentypen/)