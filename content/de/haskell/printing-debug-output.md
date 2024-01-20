---
title:                "Ausgabe von Debugging-Informationen drucken"
html_title:           "Bash: Ausgabe von Debugging-Informationen drucken"
simple_title:         "Ausgabe von Debugging-Informationen drucken"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

# Debug-Ausgabe in Haskell

## Was & Warum?
Die Druckausgabe von Debug-Informationen ist eine Methode, um Daten in Textform während der Programmausführung zu erhalten. Entwickler nutzen sie um Verständnis über den internen Zustand ihres Programms zu gewinnen - besonders hilfreich bei Fehlersuche und -behebung.

## So geht's:

In Haskell nutzen wir normalerweise die Funktion `print` für die Debug-Ausgabe. Hier ist ein einfacher Codeausschnitt und seine Ausgabe:

```Haskell
main = do
    let x = 5
    print ("Der Wert von x ist ", x)
```
Ausgabe:

```Bash
("Der Wert von x ist ",5)
```
Für komplexere Debugging-Aufgaben bietet die Bibliothek `Debug.Trace` weitergehende Funktionen, z.B. `trace`:

```Haskell
import Debug.Trace

main = do
    let x = trace "X wurde definiert" 5
    print ("Wert von x ist ", x)
```
Ausgabe:

```Bash
X wurde definiert
("Wert von x ist ",5)
```

## Vertiefung

* Historischer Kontext: Haskell, eingeführt 1990, hat sich immer auf rein funktionales Programmieren konzentriert. Eine Debug-Ausgabe, die Seiteneffekte erzeugt, war dabei lange Zeit kontrovers.

* Alternativen: Manchmal ist Logging vorzuziehen, vor allem wenn man mit asynchronen Prozessen arbeitet. Bibliotheken wie `monad-logger` können hier hilfreich sein.

* Implementierungsdetails: Funktionen wie `print` oder `trace` erzeugen Ausgaben, indem sie `IO`-Monaden verwenden. Das heißt, sie erzeugen Seiteneffekte - etwas, das in der rein funktionalen Welt von Haskell vermieden werden sollte.

## Siehe auch

* Haskell Wiki über Debuggen: https://wiki.haskell.org/Debugging
* Haskell Bibliothek "Debug.Trace": https://hackage.haskell.org/package/base-4.14.1.0/docs/Debug-Trace.html
* Monaden in Haskell: https://wiki.haskell.org/Monad
* Bibliothek "monad-logger": https://hackage.haskell.org/package/monad-logger