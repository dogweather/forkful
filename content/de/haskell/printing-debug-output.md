---
title:                "Debug-Ausgaben drucken"
date:                  2024-01-20T17:52:55.840433-07:00
model:                 gpt-4-1106-preview
simple_title:         "Debug-Ausgaben drucken"

category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Drucken von Debug-Ausgaben ist das Anzeigen von Variablen, Zwischenständen oder Hinweisnachrichten während des Programmablaufs. Programmierer nutzen dies, um das Verhalten ihres Codes zu verstehen und Fehler zu finden.

## So Geht's:
```haskell
main :: IO ()
main = do
  let zahl = 42
  putStrLn $ "Die Zahl ist: " ++ show zahl
  -- Direktes Debugging mit einer einfachen Ausgabe
```
Ausgabe:
```
Die Zahl ist: 42
```
Nutzen Sie `Debug.Trace` für Nebenläufigkeits-Debugging:
```haskell
import Debug.Trace (trace)

main :: IO ()
main = do
  let ergebnis = trace "Hier wird addiert." (1 + 1)
  putStrLn $ "Das Ergebnis ist: " ++ show ergebnis
```
Ausgabe:
```
Hier wird addiert.
Das Ergebnis ist: 2
```

## Vertiefung:
Das Drucken von Debug-Ausgaben ist ein altbekanntes Verfahren, das seit den Anfängen der Programmierung verwendet wird, um den Ablauf von Programmen nachzuvollziehen. In Haskell ist das direkte Drucken mit `putStrLn` etwas Besonderes, weil Haskell eine rein funktionale Sprache ist. Side-Effekte, wie das Drucken auf die Konsole, werden in einem `IO`-Kontext gehandhabt.

Es gibt Alternativen zum Einbau von Debug-Ausgaben direkt im Code: Interaktive Debugger wie `ghci` ermöglichen das Setzen von Breakpoints und das Inspektieren von Variablen ohne Code-Änderungen. Auch Tools wie `Trace` oder `Debug.Trace` sind praktisch, da sie es ermöglichen, Werte zu protokollieren, ohne den Programmfluss zu ändern. Im Produktionscode sollten sie trotzdem vermieden werden.

Implementierungsdetails hängen von der Wahl der Funktion oder des Werkzeugs ab: Während `putStrLn` klar dafür gedacht ist, Strings auszugeben, ermöglicht `Debug.Trace` das Einfügen von Debug-Meldungen, die bei der Auswertung von Ausdrücken angezeigt werden, was in der rein funktionalen Welt Haskell's besonders nützlich ist.

## Siehe Auch:
- [Learn You a Haskell for Great Good! (Debugging)](http://learnyouahaskell.com/)
- [Haskell Debugging with GHCI](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ghci.html)
- [Hackage: Debug.Trace](https://hackage.haskell.org/package/base-4.16.1.0/docs/Debug-Trace.html)
