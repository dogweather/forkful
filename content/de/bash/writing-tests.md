---
title:                "Tests schreiben"
html_title:           "Bash: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/writing-tests.md"
---

{{< edit_this_page >}}

# Warum & Wofür?
Schreiben von Tests ist ein Teil des Entwicklungsprozesses: Es besteht aus der Erstellung und Ausführung von Code, der überprüft, ob die Funktionalität des geschriebenen Codes korrekt ist. Programmierer tun dies, um sicherzustellen, dass ihr Code effektiv und fehlerfrei ist, bevor er bei der Entwicklung von Software oder Anwendungen eingesetzt wird.

# Wie gehts?
Tests in Bash zu schreiben ist relativ einfach und erfordert nur eine grundlegende Kenntnis von Bash-Skripten. Hier ist ein einfaches Beispiel für ein Beispielskript und eine Testdatei:

```Bash
# Beispiel-Skript, das zwei Zahlen addiert
add_numbers.sh
#!/bin/bash
echo "Geben Sie die erste Zahl ein:"
read num1
echo "Geben Sie die zweite Zahl ein:"
read num2
sum=$((num1+num2))
echo "Das Ergebnis ist: $sum"
```

```Bash
# Testdatei für das Beispiel-Skript
test_add_numbers.sh
#!/bin/bash
sum=$(./add_numbers.sh << EOF
5
10
EOF
)
if [ "$sum" -eq 15 ]; then
  echo "Test erfolgreich!"
else
  echo "Test fehlgeschlagen!"
fi
```

Die Testdatei gibt den Befehl `./add_numbers.sh` an das Skript weiter, um die Ausgabe zu generieren. Anschließend wird geprüft, ob das Ergebnis tatsächlich 15 ist. Wenn ja, ist der Test erfolgreich, andernfalls ist der Test fehlgeschlagen.

# Tiefergehende Informationen
Das Schreiben von Tests in Bash ist Teil des Test Driven Development (TDD) Konzepts, bei dem Tests vor der eigentlichen Entwicklung geschrieben werden. Dies stellt sicher, dass der Code funktioniert, bevor er implementiert wird. Alternativen zum Schreiben von Tests mit Bash sind die Verwendung von Testframeworks wie `Bats` oder die Integration von Unit-Tests in den Code selbst. Die Implementierung von Tests in Bash kann auch automatisiert werden, um den Prozess effizienter zu gestalten.

# Weitere Informationen
- [Test Driven Development (TDD)](https://de.wikipedia.org/wiki/Testgetriebene_Entwicklung)
- [Bats - Bash Automated Testing System](https://github.com/bats-core/bats-core)
- [The Bash Guide - Writing Test Suites](https://guide.bash.academy/development/testing.html)