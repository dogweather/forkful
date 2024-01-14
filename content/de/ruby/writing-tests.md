---
title:                "Ruby: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

In der Welt der Softwareentwicklung gibt es eine Vielzahl von Programmiersprachen und -methoden, die verwendet werden können, um qualitativ hochwertigen Code zu erstellen. Eine davon ist Ruby, eine dynamische Programmiersprache, die für ihre Benutzerfreundlichkeit und Lesbarkeit bekannt ist. Aber warum sollte man Tests in Ruby schreiben? Nun, Tests helfen dabei, Fehler in unserem Code zu finden und zu beheben, bevor sie sich auf die Funktionalität unserer Anwendung auswirken. Sie ersparen uns auch Zeit und Mühe, da wir nicht manuell jeden Bereich unseres Codes überprüfen müssen. In diesem Artikel werden wir uns genauer damit befassen, wie man Tests in Ruby schreibt und warum sie für jeden Entwickler unerlässlich sind.

## Wie man Tests in Ruby schreibt

Die Syntax für Tests in Ruby ist einfach und intuitiv. In der Regel verwendet man dazu das Ruby Testing Framework "MiniTest", das in der Standardbibliothek von Ruby enthalten ist. Um eine Testklasse zu erstellen, müssen wir zunächst eine neue Datei mit der Dateiendung ".rb" erstellen und sie in unserer Ruby-Anwendung importieren. Innerhalb dieser Datei erstellen wir dann eine Klasse mit dem Namen "Test" und erweitern sie von der Klasse "Minitest::Test". Hier ist ein Beispiel:

```Ruby
require 'minitest/autorun'

class Test < Minitest::Test
  # Testmethoden hier einfügen
end
```
Nun können wir Testmethoden innerhalb dieser Klasse definieren, die jeweils mit "test_" beginnen. Hier ist ein Beispiel für eine Testmethode, die überprüft, ob die Summe von zwei Zahlen korrekt berechnet wird:

```Ruby
def test_summe_von_zwei_zahlen
  assert_equal 7, 3 + 4  # Expected, Actual
end
```

Wir verwenden hier die Methode "assert_equal", um zu überprüfen, ob die erwartete Ausgabe mit der tatsächlichen Ausgabe übereinstimmt. Wenn dies nicht der Fall ist, wird der Test fehlschlagen und uns mitteilen, wo der Fehler aufgetreten ist. Es gibt auch andere nützliche Methoden für das Testen von Code, wie zum Beispiel "assert_nil" oder "assert_raises". Eine vollständige Liste findet man in der Dokumentation von MiniTest.

## Deep Dive

Nun, da wir wissen, wie man Tests in Ruby schreibt, lassen Sie uns etwas tiefer in das Konzept des Testens eintauchen. Tests sind in der Regel in zwei Kategorien unterteilt: Unit-Tests und Integrationstests. Unit-Tests überprüfen einzelne Methoden oder Klassen, während Integrationstests die Zusammenspiel von verschiedenen Komponenten unserer Anwendung testen. Beide sind wichtig, um sicherzustellen, dass unser Code gut getestet und zuverlässig ist.

Eine gute Praxis beim Schreiben von Tests ist auch das "Arrange-Act-Assert"-Muster, bei dem wir den Code in drei Teile unterteilen: das Anordnen (Arrange), das Ausführen (Act) und das Überprüfen (Assert). Das Anordnen bezieht sich auf das Einrichten von Vorbedingungen für unseren Test, das Ausführen auf die Ausführung der zu testenden Methode und das Überprüfen auf das Überprüfen der erwarteten Ausgabe.

Eine weitere wichtige Sache beim Testen ist die Codeabdeckung. Das bedeutet, wie viel Prozent unseres Codes durch Tests abgedeckt wird. Wir sollten immer versuchen, eine hohe Codeabdeckung zu erreichen, um sicherzustellen, dass unser Code gut getestet ist.

## Siehe auch

- Einführung in MiniTest: https://www.rubyguides.com/2018/07/minitest/
- Ruby Testing Guide: https://www.rubyguides.com/2018/07/make-ruby-tests/
- Die offizielle MiniTest Dokumentation: https://ruby-doc.org/stdlib-2.4.1/libdoc/minitest/rdoc/MiniTest.html