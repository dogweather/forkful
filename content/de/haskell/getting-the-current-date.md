---
title:    "Haskell: Das aktuelle Datum erhalten"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# Warum

Das aktuelle Datum und die Uhrzeit sind wichtige Informationen in der Programmierung. Sie können verwendet werden, um in Anwendungen zeitbasierte Funktionen zu implementieren oder einfach nur, um den Benutzern das aktuelle Datum anzuzeigen. In diesem Blogpost zeige ich Ihnen, wie Sie mithilfe von Haskell das aktuelle Datum und die Uhrzeit abrufen können.

# Wie geht das?

Zunächst müssen wir das `Data.Time` Modul in unser Programm importieren. Dann können wir die `getCurrentTime` Funktion verwenden, um das aktuelle Datum und die Uhrzeit zu erhalten. Hier ist ein Beispiel:

```Haskell
import Data.Time

main = do
  now <- getCurrentTime
  putStrLn $ "Das aktuelle Datum und die Uhrzeit sind: " ++ show now
```

Die `getCurrentTime` Funktion gibt ein Objekt vom Typ `UTCTime` zurück, das das Datum und die Uhrzeit im UTC-Format (koordinierte Weltzeit) enthält. Wenn wir diese Information in ein menschenlesbares Format umwandeln möchten, können wir die `formatTime` Funktion verwenden. Hier ist ein Beispiel, das das Datum und die Uhrzeit im Format "dd.mm.yyyy HH:MM" ausgibt:

```Haskell
-- Wir nutzen die Funktion `formatTime` aus dem `Data.Time.Format` Modul
import Data.Time.Format

-- Wir verwenden die `show` Funktion, um das `UTCTime` Objekt in einen String umzuwandeln
main = do
  now <- getCurrentTime
  putStrLn $ "Das aktuelle Datum und die Uhrzeit sind: " ++ show (formatTime defaultTimeLocale "%d.%m.%Y %H:%M" now)
```

Die Ausgabe dieses Beispiels wäre beispielsweise: "Das aktuelle Datum und die Uhrzeit sind: 09.08.2021 15:30".

# Tiefere Einblicke

Möchten Sie das Datum und die Uhrzeit in einer bestimmten Zeitzone oder mit einem anderen Format erhalten? Das ist mit Haskell ebenfalls möglich. Hier ist ein Beispiel, das das Datum und die Uhrzeit in der Zeitzone "Europe/Berlin" sowie das Datum im Format "Wochentag, dd.mm.yyyy" ausgibt:

```Haskell
import Data.Time
import Data.Time.Zone

-- Wir erstellen eine Variable mit der gewünschten Zeitzone
berlinTimeZone = hoursToTimeZone 2

main = do
  now <- getCurrentTime
  -- Hier passen wir die `formatTime` Funktion an, um sowohl die Zeitzone als auch ein individuelles Format zu verwenden
  putStrLn $ "Das aktuelle Datum und die Uhrzeit in Berlin sind: " ++ show (formatTime defaultTimeLocale "%d.%m.%Y" (utcToLocalTime berlinTimeZone now)) ++ " " ++ show (formatTime defaultTimeLocale "%A" now)
```

Die Ausgabe dieses Beispiels wäre beispielsweise: "Das aktuelle Datum und die Uhrzeit in Berlin sind: 09.08.2021 Montag".

# Siehe auch

- [Dokumentation für das `Data.Time` Modul](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Weiterführende Informationen zur Zeitberechnung in Haskell](https://wiki.haskell.org/Time_and_date)
- [Eine Anleitung zum Umgang mit verschiedenen Zeitzonen in Haskell](https://alvinalexander.com/source-code/haskell/haskell-date-time-zone-how-to-get-current-time/)