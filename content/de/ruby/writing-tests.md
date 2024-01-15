---
title:                "Test schreiben"
html_title:           "Ruby: Test schreiben"
simple_title:         "Test schreiben"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Tests sind unerlässlich, um die Qualität und Zuverlässigkeit von Ruby-Code sicherzustellen. Mit Tests können Fehler frühzeitig erkannt und behoben werden, was wiederum zu einem reibungsloseren Arbeitsablauf und einem insgesamt besseren Endprodukt führt.

## Wie man Tests schreibt

Tests in Ruby werden mit dem beliebten Framework RSpec geschrieben. Hier ist ein einfaches Beispiel für einen Test, der überprüft, ob eine Methode die richtige Ausgabe zurückgibt.

```ruby
def add(a, b)
  a + b
end

describe "#add" do
  it "adds two numbers correctly" do
    result = add(2, 3)
    expect(result).to eq(5)
  end
end
```

In diesem Beispiel erstellen wir eine Methode namens `add`, die zwei Zahlen addiert. Im Test überprüfen wir, ob die Methode die richtige Ausgabe zurückgibt, in diesem Fall 5. Wenn der Test erfolgreich ist, wird die grüne Farbe in der Konsole angezeigt, was bedeutet, dass unser Code ordnungsgemäß funktioniert. Wenn es zu einem Fehler kommt, wird die rote Farbe angezeigt und wir wissen, dass etwas behoben werden muss.

Ein weiteres nützliches Feature von RSpec ist die Möglichkeit, sogenannte Mocks und Stubs zu verwenden, um externe Abhängigkeiten in den Tests zu simulieren. Dadurch können wir unsere Tests unabhängig von externen Ressourcen durchführen und garantieren, dass sie immer konsistent sind.

## Tiefere Einblicke

Eine gute Praxis beim Schreiben von Tests ist es, jeden Aspekt des Codes abzudecken und sicherzustellen, dass alle möglichen Szenarien getestet werden. Dies wird als "Testabdeckung" bezeichnet und kann mit Tools wie SimpleCov gemessen werden. Eine hohe Testabdeckung gibt uns Vertrauen in unseren Code und hilft uns, potenzielle Fehler frühzeitig zu erkennen.

Es gibt auch verschiedene Arten von Tests, wie zum Beispiel Unit Tests, Integrationstests und End-to-End-Tests. Das Verständnis der Unterschiede und wann sie angewendet werden sollten, kann sehr hilfreich sein, um die Teststrategie zu optimieren.

## Siehe auch

- [RSpec Dokumentation](https://rspec.info/documentation/)
- [Einführung in Test-Driven Development (TDD)](https://medium.com/@waqarahmadjustdoit/test-driven-development-tdd-an-introduction-4c91ba038cea)
- [Die Bedeutung von Testabdeckung](https://medium.com/swlh/what-is-code-coverage-and-why-should-you-care-14b21a16fca1)