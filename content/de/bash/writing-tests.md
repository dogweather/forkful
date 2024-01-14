---
title:    "Bash: Tests schreiben"
keywords: ["Bash"]
---

{{< edit_this_page >}}

# Warum

Tests sind eine wichtige Komponente in der Softwareentwicklung. Sie helfen dabei, Bugs frühzeitig zu erkennen und die Qualität des Codes zu verbessern. Durch das Schreiben von Tests können Entwickler:innen sicherstellen, dass ihr Code wie erwartet funktioniert und auch bei zukünftigen Änderungen stabil bleibt.

## So geht's

Um Tests in Bash zu schreiben, kannst du das Bats-Testing-Framework verwenden. Bats steht für "Bash Automated Testing System" und bietet eine einfache und effektive Möglichkeit, Tests in Bash-Skripten zu schreiben.

Folgende Schritte helfen dir dabei, Bats in dein Projekt zu integrieren:

1. Installiere Bats über folgenden Befehl: `npm install -g bats`
2. Erstelle einen Ordner mit dem Namen "tests" oder "specs" in deinem Projektverzeichnis.
3. Schreibe deine Tests in diesem Ordner mit der Dateiendung ".bats".

Ein einfaches Beispiel für einen Test mit Bats könnte wie folgt aussehen:

```Bash
#!/usr/bin/env bats

@test "1 plus 1 equals 2" {
  result=$(($1 + $2))
  [ $result -eq 2 ]
}
```

Führe den Test aus, indem du den Befehl `bats tests/test_example.bats` in deiner Konsole ausführst. Das Ergebnis sollte wie folgt aussehen:

```
 ✓ 1 plus 1 equals 2

1 test, 0 failures
```

Herzlichen Glückwunsch, du hast erfolgreich deinen ersten Test in Bash geschrieben!

## Tiefer eintauchen

Das Bats-Framework bietet viele nützliche Funktionen, um Tests noch effektiver zu gestalten. Zum Beispiel kannst du mit der Funktion `skip` Tests überspringen oder mit der Funktion `setup` vor jedem Test eine Aktion ausführen.

Außerdem ermöglicht Bats das Testen von Ausgaben, die auf die Standardausgabe oder den Standardfehler streamen. Mit der Funktion `run` kannst du ein Skript ausführen und dann mit der Funktion `result` überprüfen, ob die erwartete Ausgabe zurückgegeben wurde.

Eine detaillierte Dokumentation und weitere Beispiele findest du auf der offiziellen Bats-Website: [https://github.com/bats-core/bats-core](https://github.com/bats-core/bats-core)

## Siehe auch

- [Bats GitHub Repository](https://github.com/bats-core/bats-core)
- [Offizielle Bats Dokumentation](https://bats-core.readthedocs.io/en/latest/)