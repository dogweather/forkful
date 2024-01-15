---
title:                "Schreiben auf die Standardfehlerausgabe"
html_title:           "Haskell: Schreiben auf die Standardfehlerausgabe"
simple_title:         "Schreiben auf die Standardfehlerausgabe"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Schreiben auf die Standardfehler-Ausgabe ist eine wichtige Fähigkeit für jeden, der in Haskell programmiert. Es ermöglicht uns, Fehlermeldungen und Warnungen zur Laufzeit auszugeben, was beim Debuggen und Testen unserer Programme hilft.

## Wie geht das?

Es gibt mehrere Möglichkeiten, auf die Standardfehler-Ausgabe in Haskell zuzugreifen. Eine Möglichkeit ist die Verwendung der Funktion `hPutStrLn` aus dem `System.IO` Modul. Diese Funktion erwartet einen Handle für die Standardfehler-Ausgabe und einen String als Eingabe. Zum Beispiel:

```Haskell
import System.IO

main = do
    hPutStrLn stderr "Dies ist eine Fehlermeldung."
```

Dieses Beispiel gibt den String "Dies ist eine Fehlermeldung." auf die Standardfehler-Ausgabe aus. Wir können auch Variablen oder mathematische Operationen in den String einfügen, wie in diesem Beispiel:

```Haskell
import System.IO

main = do
    let x = 5
    hPutStrLn stderr ("Die quadratische Wurzel von " ++ show x ++ " ist " ++ show (sqrt x) ++ ".")
```

Dieses Beispiel gibt den String "Die quadratische Wurzel von 5 ist 2.23606797749979." auf die Standardfehler-Ausgabe aus.

Eine andere Möglichkeit ist die Verwendung der `error` Funktion aus dem `Prelude` Modul. Diese Funktion erwartet einen String als Eingabe und beendet das Programm mit einer entsprechenden Fehlermeldung auf der Standardfehler-Ausgabe. Zum Beispiel:

```Haskell
main = error "Ups, hier ist etwas schief gelaufen."
```

Dieses Beispiel beendet das Programm mit der Fehlermeldung "Ups, hier ist etwas schief gelaufen." auf der Standardfehler-Ausgabe.

## Tiefer Einblick

Es gibt noch viele weitere Funktionen und Techniken, um auf die Standardfehler-Ausgabe zuzugreifen und sie effektiv zu nutzen. Zum Beispiel können wir auch den `stderr` Handle aus dem `System.IO` Modul direkt verwenden, ohne die Funktion `hPutStrLn` zu benutzen. Wir können auch Farbcodes verwenden, um die Ausgabe auf der Standardfehler-Ausgabe hervorzuheben.

Die Standardfehler-Ausgabe kann auch hilfreich sein, um Benutzereingaben zu validieren oder zusätzliche Informationen während der Ausführung eines Programms anzuzeigen.

## Siehe auch

- [Haskell Dokumentation zu System.IO](https://www.haskell.org/onlinereport/io.html)
- [Ein Artikel über die `error` Funktion](https://wiki.haskell.org/Partial_application#Prelude_error)
- [Ein praktisches Beispiel zur Verwendung der Standardfehler-Ausgabe](https://stackoverflow.com/questions/36926077/using-stderr-for-debugging-in-haskell)