---
title:                "Lesen von Befehlszeilenargumenten"
html_title:           "Haskell: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Was & Warum?

Das Lesen von Befehlszeilenargumenten ist eine Möglichkeit, Eingaben von Benutzern direkt über die Kommandozeile zu erhalten. Programmierer nutzen diese Funktion, um ihre Programme flexibler zu gestalten und Benutzerinteraktion zu ermöglichen.

# Wie geht das?

```Haskell
import System.Environment

main = do
    args <- getArgs
    putStrLn $ "Die übergebenen Argumente lauten: " ++ unwords args
```

Dieses Beispiel zeigt, wie mit dem 'System.Environment'-Modul die Befehlszeilenargumente in Haskell ausgelesen werden können. Mit der 'getArgs'-Funktion werden alle Argumente als Liste von Strings zurückgegeben. Diese können dann durch die 'unwords'-Funktion zu einem einzelnen String zusammengefügt und ausgegeben werden.

Beispiel-Ausgabe:
```Haskell
Die übergebenen Argumente lauten: Hallo Welt!
```

# Tiefer schauen

Das Konzept des Lesens von Befehlszeilenargumenten stammt aus den Anfängen der Programmierung, als interaktive Echtzeitprogramme in der Regel über die Kommandozeile ausgeführt wurden. Mittlerweile gibt es jedoch auch alternative Möglichkeiten, Eingaben von Benutzern zu erhalten, wie z.B. über grafische Benutzeroberflächen.

Die Implementierung dieser Funktion in Haskell basiert auf dem Unix-orientierten Betriebssystem, wo die Befehlszeilenargumente als strings in eine Liste gespeichert werden. 

# Siehe auch

Für weitere Informationen und Beispiele zur Verwendung von Befehlszeilenargumenten in Haskell, folgen Sie den folgenden Links:

- Offizielle Dokumentation des 'System.Environment'-Moduls von Haskell: https://hackage.haskell.org/package/base/docs/System-Environment.html
- "Command-line Arguments in Haskell" von KenopoTech: https://www.fpcomplete.com/blog/2017/09/command-line-arguments-in-haskell