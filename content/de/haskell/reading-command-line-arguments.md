---
title:    "Haskell: Lesen von Befehlszeilen Argumenten"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Warum

Das Lesen von Befehlszeilenargumenten ist eine wichtige Fähigkeit für jeden Haskell-Programmierer. Es ermöglicht die Übergabe von Informationen an ein Programm auf einfache und effiziente Weise.

# Wie geht das?

Um Befehlszeilenargumente in Haskell zu lesen, müssen wir die Funktion `getArgs` aus dem Modul `System.Environment` importieren. Diese Funktion gibt eine Liste mit den Argumenten zurück, die beim Aufruf des Programms übergeben wurden.

```Haskell
import System.Environment

main = do
    args <- getArgs
    putStrLn ("Erhaltene Argumente: " ++ show args)
```

Wenn wir dieses Programm mit dem Befehl `runghc programm.hs arg1 arg2 arg3` ausführen, wird die Ausgabe folgendermaßen aussehen:

```
Erhaltene Argumente: ["arg1", "arg2" , "arg3"]
```

Beachten Sie, dass der erste argument im output der Name des Programms ist, der dort automatisch eingefügt wird.

# Tief in die Materie eintauchen

Wenn wir uns genauer mit dem Versuch befassen, Befehlszeilenargumente zu lesen, können wir sehen, dass die Funktion `getArgs` in Haskell auf der Funktion `getProgArgs` basiert, die wiederum auf dem `System.IO`-Modul basiert. Durch die Kombination all dieser Funktionen wird das Parsen der Befehlszeilenargumente sehr effizient, da sie direkt auf dem Betriebssystem aufbauen.

# Siehe auch

- Dokumentation zu `System.Environment` - https://hackage.haskell.org/package/base-4.14.1.0/docs/System-Environment.html
- Offizielle Haskell-Dokumentation - https://www.haskell.org/documentation/
- Ein Tutorial zum Lesen von Befehlszeilenargumenten in Haskell - https://wiki.haskell.org/Command_line_argument_handling