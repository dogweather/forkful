---
title:                "Programmieren von Tests"
html_title:           "Ruby: Programmieren von Tests"
simple_title:         "Programmieren von Tests"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/writing-tests.md"
---

{{< edit_this_page >}}

# Was & Warum?

Tests schreiben ist ein wichtiger Teil des Programmierens, bei dem man Code auf seine Funktionalität und Korrektheit überprüft. Durch das Schreiben von Tests können Programmierfehler frühzeitig erkannt und behoben werden, was zu einer besseren Codequalität und Robustheit führt.

# Wie geht's:

Hier sind zwei Beispiele, wie man Tests in Ruby schreiben kann:

```Ruby
require 'minitest/autorun'
class Calculator
  def add(x, y)
    return x + y
  end
end
class TestCalculator < Minitest::Test
  def test_add
    calc = Calculator.new
    assert_equal 5, calc.add(2, 3)
  end
end
```

Die Ausgabe des Tests sollte "  " lauten.

```Ruby
require 'minitest/autorun'
class User
  attr_accessor :name
  def initialize(name)
    @name = name
  end
end
class TestUser < Minitest::Test
  def test_initialize
    user = User.new("John")
    assert_equal "John", user.name
  end
end
```

Die Ausgabe dieses Tests sollte "true" lauten.

# Tiefere Einblicke:

Tests zu schreiben ist nicht nur eine moderne Praxis, sondern hat auch eine interessante historische Bedeutung. Das Testen von Code war bereits in den frühen Tagen der Softwareentwicklung wichtig, um die Qualität des Codes zu gewährleisten und Probleme frühzeitig zu identifizieren. Alternativen zu Ruby's Minitest sind beispielsweise RSpec oder Cucumber. Auch die Verwendung von Test-Driven Development ist eine beliebte Methode im Ruby-Ökosystem. Die Implementierung von Tests in Ruby erfolgt mithilfe von Assertions und Mocking-Bibliotheken wie MiniTest::Assertions und MiniTest::Mock.

# Sieh dir auch an:

- [Minitest Dokumentation](https://github.com/seattlerb/minitest)
- [RSpec](https://rspec.info/)
- [Cucumber](https://cucumber.io/)
- [Test-Driven Development in Ruby](https://www.rubyguides.com/2018/07/test-driven-development-by-example/)