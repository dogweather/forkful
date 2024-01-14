---
title:                "Elm: Ein neues Projekt beginnen"
programming_language: "Elm"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Warum
Hast du schon mal darüber nachgedacht, eine neue Programmiersprache auszuprobieren? Elm könnte die Antwort sein! Mit ihren funktionellen Eigenschaften und der Fokussierung auf fehlerfreie Code-Erstellung bietet sie eine spannende Alternative zu den klassischen Programmiersprachen. In diesem Blogpost werden wir uns näher mit dem Einstieg in Elm beschäftigen und dir einen Einblick in seine Funktionalität geben.

# So geht's
Um eine neue Elm-Projekt zu starten, brauchst du zunächst den Elm-Compiler. Diesen kannst du dir auf der offiziellen Webseite herunterladen. Nach der Installation kannst du ein neues Projektverzeichnis erstellen und darin eine *Main.elm* Datei anlegen. Nun kannst du mit dem Programmieren beginnen!

Hier ist ein einfaches Beispiel für eine Funktion, die die Summe zweier Zahlen berechnet und ausgibt:

```elm
summe z1 z2 = z1 + z2

main =
    let
        ersteZahl = 5
        zweiteZahl = 10
    in
    Debug.log "Die Summe ist: " (summe ersteZahl zweiteZahl)
```

Wie du sehen kannst, werden die möglichen Ausgaben in Elm mit der Funktion *Debug.log* angezeigt. Diese Funktion ist nur in der Entwicklungsumgebung möglich und wird später automatisch entfernt.

# Tiefer tauchen
Wenn du tiefer in die Welt von Elm eintauchen möchtest, kannst du dir die offizielle Dokumentation auf der Webseite ansehen. Dort findest du viele nützliche Informationen zu den Funktionen und Eigenschaften von Elm. Es gibt auch eine aktive Community, in der du Hilfe und Unterstützung von anderen Entwicklern bekommen kannst.

Außerdem empfehlen wir dir, Elm in kleinen Projekten auszuprobieren, um dich mit der Syntax und den Funktionen vertraut zu machen. Eine gute Möglichkeit dafür ist das "Elm Architecture Tutorial", das dir Schritt für Schritt zeigt, wie du eine einfache ToDo-Liste mit Elm erstellen kannst.

# Siehe auch
- [Offizielle Elm Webseite](https://elm-lang.org/)
- [Elm Dokumentation und Community](https://guide.elm-lang.org/)
- [Elm Architecture Tutorial](https://www.elm-tutorial.org/)