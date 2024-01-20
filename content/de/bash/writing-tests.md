---
title:                "Tests schreiben"
html_title:           "Arduino: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Schreiben von Tests bedeutet, Skripte zu erstellen, die Code auf korrekte Funktion überprüfen. Programmierer tun dies, um Fehler frühzeitig zu erkennen und die Softwarequalität sicherzustellen.

## How to:

```Bash
#!/bin/bash

# Funktion, die getestet wird.
addNumbers() {
    echo $(($1 + $2))
}

# Test der addNumbers Funktion.
testAddNumbers() {
    result=$(addNumbers 3 4)
    if [ "$result" -eq 7 ]; then
        echo "Test erfolgreich: 3 + 4 ist gleich $result"
    else
        echo "Test fehlgeschlagen: 3 + 4 sollte 7 sein, aber wir haben $result"
    fi
}

# Test ausführen.
testAddNumbers
```

Beispielausgabe:

```
Test erfolgreich: 3 + 4 ist gleich 7
```

## Deep Dive

Historischer Kontext: Das Konzept des Testens von Software entstand in den 1950er Jahren, parallel zur Entwicklung komplexer Systeme. Damals wie heute sind Tests unverzichtbar zur Qualitätssicherung. Alternativen zum Bash-Skript-Testen umfassen Frameworks wie shUnit2 oder Bats, die speziellere Funktionalitäten bieten. Bei der Implementierung ist wichtig, dass Tests isoliert, wiederholbar und aussagekräftig gestaltet werden.

## See Also

- [Bats-core: Bash Automated Testing System](https://github.com/bats-core/bats-core)
- [shUnit2: Unit testing framework for shell scripts](https://github.com/kward/shunit2)
- Artikel zu "Best Practices" im Bash-Testing: [Advanced Bash-Scripting Guide: Testing](https://tldp.org/LDP/abs/html/testing.html)