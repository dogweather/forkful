---
title:                "Haskell: Lesen von Befehlszeilenargumenten"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Wenn du dich für das Programmieren mit Haskell entschieden hast, bist du wahrscheinlich schon mit der funktionalen Programmierung und der Stärke der Typisierung vertraut. Aber wusstest du, dass Haskell auch sehr effizient mit Kommandozeilenargumenten umgehen kann? In diesem Blogbeitrag zeigen wir dir, warum es sich lohnt, sich mit dem Lesen von Kommandozeilenargumenten in Haskell auseinanderzusetzen.

## Anleitung

Das Lesen von Kommandozeilenargumenten in Haskell ist dank der "System.Environment"-Bibliothek sehr einfach. Zunächst musst du diese Bibliothek in deinem Code importieren:

```Haskell
import System.Environment
```

Als nächstes verwenden wir die Funktion "getArgs", um die Argumente zu lesen und sie der Variablen "args" zuzuweisen:

```Haskell
args <- getArgs
```

Die "args"-Variable enthält nun eine Liste der übergebenen Argumente. Um sie auszugeben, können wir die "putStrLn"-Funktion verwenden:

```Haskell
putStrLn "Die übergebenen Argumente sind:"
mapM_ putStrLn args
```

Dieser Code wird die Liste der Argumente Zeile für Zeile ausgeben. Wenn du dieses Beispiel ausführst und deinem Programm Argumente übergibst, wirst du sehen, wie einfach es ist, sie mit Haskell zu lesen. Hier ein Beispiel der Ausgabe:

```
> MeinProgramm Argument1 Argument2 Argument3
Die übergebenen Argumente sind:
Argument1
Argument2
Argument3
```

## Tiefergehende Informationen

Die "System.Environment"-Bibliothek bietet noch viele weitere Funktionen, die dir helfen können, die übergebenen Argumente weiter zu verarbeiten. Eine davon ist die "getProgName"-Funktion, mit der du den Namen deines Programms auslesen kannst. Eine andere nützliche Funktion ist "lookupEnv", mit der du Umgebungsvariablen auslesen kannst.

Es gibt auch viele weitere Bibliotheken, die dir dabei helfen können, Kommandozeilenargumente zu verarbeiten. Eine davon ist die "optparse-applicative"-Bibliothek, mit der du Optionen und Argumente parsen und verarbeiten kannst.

Das Lesen von Kommandozeilenargumenten in Haskell ist auch sehr nützlich, wenn du Skripte schreibst, die mit anderen Programmen interagieren müssen. Du kannst die Argumente, die du von der Kommandozeile erhältst, verwenden, um dein Programm anzupassen oder bestimmte Aktionen auszuführen.

## Siehe auch

- Offizielle Dokumentation zu "System.Environment": https://hackage.haskell.org/package/base/docs/System-Environment.html
- Übersicht über nützliche Bibliotheken für das Lesen von Kommandozeilenargumenten in Haskell: https://wiki.haskell.org/Applications_and_libraries/GetOpt