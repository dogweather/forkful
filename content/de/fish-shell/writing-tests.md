---
title:                "Tests schreiben"
date:                  2024-01-19
html_title:           "Arduino: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?

Tests schreiben bedeutet, Code zu erstellen, der prüft, ob andere Codeabschnitte wie erwartet funktionieren. Programmierer machen das, um Fehler früh zu erkennen, Codequalität zu sichern und spätere Wartung zu vereinfachen.

## How to:

### Einfacher Test

```Fish Shell
function test_greeting
    echo "Hallo Welt" | grep -q "Hallo"
    and echo "Test bestanden"
    or echo "Test fehlgeschlagen"
end

test_greeting
```

#### Ausgabe:

```Shell
Test bestanden
```

### Test mit erwartetem Fehler

```Fish Shell
function test_fail_example
    false # Dieser Befehl wird immer mit einem Fehler beenden
    and echo "Sollte nicht passieren"
    or echo "Erwartetes Versagen"
end

test_fail_example
```

#### Ausgabe:

```Shell
Erwartetes Versagen
```

## Deep Dive

Fish Shell umfasst nicht direkt eingebaute Testing-Frameworks wie andere Programmiersprachen. Historisch gesehen nutzen Entwickler externe Tools wie `fishtape` oder bash-basierte Frameworks. Das bietet weniger Integration aber erlaubt Anpassung und Nutzung bewährter Methoden. Tests in Fish sind normalerweise einfacher Natur und setzen auf die Shell's Fähigkeit, Kommandos und Scripts zu bewerten.

## See Also

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Fishtape, ein Test-Framework für Fish Scripts](https://github.com/jorgebucaran/fishtape)
- [Fish Shell Tutorial für Anfänger](https://fishshell.com/docs/current/tutorial.html)
