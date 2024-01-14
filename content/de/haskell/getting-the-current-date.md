---
title:                "Haskell: Das aktuelle Datum erhalten"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

In der heutigen Zeit ist es üblich, dass wir auf unserem Computer oder Smartphone jederzeit die aktuelle Uhrzeit und das Datum abrufen können. Aber hast du dich schon einmal gefragt, wie diese Informationen eigentlich auf deinem Gerät erscheinen? In dieser Blog-Post werden wir uns mit der Programmierung von Haskell beschäftigen und lernen, wie wir das aktuelle Datum in unserer Software anzeigen können.

## Wie geht das?

Zunächst müssen wir in Haskell die `Data.Time` Bibliothek importieren, die uns verschiedene Funktionen und Datenstrukturen zur Verarbeitung von Datum und Uhrzeit bietet. Ein Beispielcode dafür sieht folgendermaßen aus:

```Haskell
import Data.Time

main = do
  now <- getCurrentTime -- Speichert die aktuelle Uhrzeit in der Variable "now"
  let date = utctDay now -- Extrahiert das Datum aus der aktuellen Uhrzeit
  putStrLn $ "Heute ist der " ++ show date -- Gibt das Datum in dem String aus
```

Wenn wir diesen Code ausführen, bekommen wir als Output das aktuelle Datum in der Form `2021-10-07` angezeigt. Natürlich können wir auch andere Formate verwenden, je nachdem wie wir das Datum darstellen möchten. Dafür stehen uns verschiedene Funktionen zur Verfügung, zum Beispiel `formatTime` oder `parseTimeM`.

## Tiefere Einblicke

Um das aktuelle Datum zu verstehen, müssen wir uns etwas tiefer in die verwendeten Datenstrukturen einarbeiten. In Haskell wird das Datum in der Form `UTCTime` dargestellt, welches aus einem `Day` und einer `DiffTime` besteht. Der `Day` beinhaltet das Datum, während die `DiffTime` die Differenz zur UTC-Zeit angibt. Die Funktion `getCurrentTime` gibt uns ein `UTCTime` Objekt zurück, welches wir dann mit verschiedenen Funktionen verarbeiten können.

## Siehe auch

- [Haskell Dokumentation zu Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Beispielcode zu Datum und Uhrzeit in Haskell](https://riptutorial.com/haskell/example/18941/date-and-time)