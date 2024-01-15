---
title:                "Tests schreiben"
html_title:           "PHP: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/writing-tests.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte man sich überhaupt die Mühe machen, Tests zu schreiben? Nun, es gibt mehrere Gründe dafür. Zum einen helfen Tests dabei, mögliche Fehler in einem Programmcode zu finden und zu beheben. Dadurch wird die Qualität des Codes verbessert und die Wahrscheinlichkeit von Fehlern in der Anwendung reduziert. Zudem dienen Tests auch als Dokumentation des Codes und erleichtern somit die Arbeit für andere Entwickler, die den Code lesen oder bearbeiten müssen.

## Wie man Tests schreibt
Das Schreiben von Tests ist im Grunde genommen ganz einfach. Zunächst muss man eine Testumgebung einrichten, in der die Tests ausgeführt werden können. Dies kann zum Beispiel das PHPUnit Framework sein. Anschließend muss der Code, der getestet werden soll, in einzelne Funktionen oder Klassen aufgeteilt werden. Für jede Funktion oder Klasse wird dann eine entsprechende Testfunktion geschrieben, die überprüft, ob das erwartete Ergebnis erreicht wird. Hier ein Beispiel in PHP:

```PHP
// Die zu testende Funktion
function add($a, $b) {
  return $a + $b;
}

// Die entsprechende Testfunktion
function testAdd() {
  $result = add(3, 5);
  assert($result == 8, "Die Funktion add() liefert das falsche Ergebnis!");
}
```

Nachdem alle Testfunktionen geschrieben wurden, können sie durch die Testumgebung ausgeführt werden. Wenn alle Tests erfolgreich sind, bedeutet das, dass der Code den Erwartungen entspricht und keine Fehler aufweist. Sollte jedoch ein Test fehlschlagen, liegt höchstwahrscheinlich ein Fehler im Code vor, der behoben werden muss.

## Tiefergehende Informationen
Beim Schreiben von Tests gibt es einige wichtige Punkte zu beachten. Zum einen müssen die Tests unabhängig voneinander sein, das heißt, das Ergebnis eines Tests darf nicht von einem anderen Test beeinflusst werden. Außerdem sollten die Tests so einfach wie möglich gehalten werden, um die Lesbarkeit zu erhöhen. Eine gute Testabdeckung, also die Anzahl der getesteten Codezeilen, ist ebenfalls wichtig, um Lücken im Code aufzudecken. Zudem ist es sinnvoll, Tests frühzeitig in den Entwicklungsprozess einzubinden, um Fehler möglichst frühzeitig zu finden.

## Siehe auch
- [PHP Testing with PHPUnit](https://phpunit.de/)
- [The Art of Unit Testing by Roy Osherove](https://artofunittesting.com/)