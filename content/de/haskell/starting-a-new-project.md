---
title:                "Haskell: Ein neues Projekt beginnen."
simple_title:         "Ein neues Projekt beginnen."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Warum

Vielleicht hast du schon von der funktionalen Programmierung gehört und bist neugierig geworden, wie sie funktioniert. Oder du möchtest deine Fähigkeiten auf dem Gebiet von Haskell verbessern. Wie auch immer, das Starten eines neuen Haskell-Projekts kann eine aufregende und lohnende Herausforderung sein.

## Wie

Zunächst benötigst du einen Texteditor und das Haskell-Toolstack. Öffne deinen Terminal und gib den Befehl `stack new my-project` ein, wobei `my-project` der Name deines Projekts ist. Wähle dann eine der vorgegebenen Projektschablonen aus, indem du eine Zahl eingibst.

Nachdem das Projekt erstellt wurde, navigiere in den Ordner und öffne die Datei `app/Main.hs`. Hier kannst du deine Haskell-Programmierung starten. Schreibe beispielsweise folgenden Code in die Datei:

````Haskell
main :: IO ()
main = do
  putStrLn "Hallo Welt!"
````

Speichere die Datei und führe den Befehl `stack run` aus, um dein Programm auszuführen. Du solltest den Text "Hallo Welt!" in der Konsole ausgegeben sehen.

## Vertiefung

Das Starten eines neuen Haskell-Projekts gibt dir die Möglichkeit, die Sprache zu erkunden und neue kreative Anwendungen zu entwickeln. Du kannst auch vorhandene Bibliotheken und Frameworks nutzen, um deine Projekte noch effektiver zu gestalten.

Denke daran, dass du dir Zeit nehmen solltest, um die grundlegenden Konzepte von Haskell zu verstehen, bevor du dich in komplexere Projekte stürzt. Finde eine gute Ressource oder ein Programmierforum, um Unterstützung und Ratschläge von erfahrenen Entwicklern zu erhalten.

## Siehe auch

- [Haskell-Dokumentation](https://www.haskell.org/documentation/)
- [Haskell-Programmierforum](https://stackoverflow.com/questions/tagged/haskell)
- [Haskellers-Community](https://www.haskell.org/community/)