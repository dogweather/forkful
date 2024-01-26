---
title:                "Schreiben auf Standardfehler"
html_title:           "Arduino: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
In der Programmierung ist das Schreiben auf den Standard Error (stderr) wichtig zum Melden von Fehlern und Diagnostik, ohne die Standardausgabe (stdout) zu stören. Das hilft dabei, Fehlermeldungen von normalen Ausgaben in Logs oder Dateien zu trennen.

## How to:
In Haskell, du benutzt `hPutStrLn` von `System.IO`, um auf stderr zu schreiben:

```Haskell
import System.IO

main :: IO ()
main = hPutStrLn stderr "Das ist ein Fehler!"
```

Ausgabe im stderr-Stream:

```
Das ist ein Fehler!
```

Schreiben von regularer Ausgabe und Fehler in unterschiedliche Streams:

```Haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stdout "Normale Ausgabe"
  hPutStrLn stderr "Dies wurde auf stderr ausgegeben"
```

Sample output:

```
Normale Ausgabe
Dies wurde auf stderr ausgegeben
```

## Deep Dive
Stderr wurde mit Unix eingeführt und ist heutzutage Standard. Alternativ zu `hPutStrLn` gibt's `io.stderr:write` und `System.Log.Logger`, um Fehlern zu loggen. Haskell's Typsystem und reine Funktionen reduzieren die Notwendigkeit von stderr-Einsatz, aber es ist immer noch nützlich bei IO-Operationen und für Command-Line Tools.

In Haskell ist die Implementierung bequem, da `stderr` bereits ein Handle in der `System.IO` Bibliothek ist. Kein manuelles Erstellen von Handles nötig – du schreibst direkt auf das vorhandene Handle.

## See Also
Für mehr Details sieh dir die offizielle Dokumentation an:
- Haskell's `System.IO` Modul: [https://hackage.haskell.org/package/base/docs/System-IO.html](https://hackage.haskell.org/package/base/docs/System-IO.html)

Entdecke weitere Funktionen von `System.IO` für erweiterte IO-Operationen.
