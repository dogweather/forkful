---
title:                "Tests schreiben"
html_title:           "Python: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/writing-tests.md"
---

{{< edit_this_page >}}

## Warum
Der Gedanke, Tests für den eigenen Code zu schreiben, kann zunächst etwas lästig erscheinen. Doch Tests haben viele Vorteile: Sie stellen sicher, dass der Code fehlerfrei funktioniert, vereinfachen die Fehlersuche und ermöglichen es, die Funktionalität bei zukünftigen Änderungen schnell zu überprüfen. Es lohnt sich also, Zeit in das Schreiben von Tests zu investieren.

## Wie geht's
Das Schreiben von Tests ist in Python sehr einfach. Hier ist ein Beispiel, wie man eine einfache Funktion testen kann:

```Python
def add(x, y):
  return x + y

# Testfall
assert add(2, 3) == 5
```

Die "assert" Anweisung überprüft, ob die Funktion den erwarteten Wert zurückgibt. Wenn der Test fehlschlägt, wird ein Fehler gemeldet.

## Tiefergehende Einblicke
Es gibt verschiedene Arten von Tests, die man schreiben kann, z.B. Modul- oder Integrationstests. Auch das Schreiben von Mock- und Unittests wird immer wichtiger. Es ist wichtig, die geeigneten Tests für den jeweiligen Code auszuwählen und diese regelmäßig auszuführen, um sicherzustellen, dass der Code fehlerfrei bleibt. Es gibt auch verschiedene Test-Frameworks, die das Schreiben von Tests erleichtern, z.B. das beliebte "pytest" oder "unittest" Modul aus der Python Standardbibliothek.

## Siehe auch
- [Die Vorteile von Tests in Python (auf Englisch)](https://chrisyeh96.github.io/2017/08/08/definitive-guide-python-imports.html)
- [Python pytest Dokumentation](https://docs.pytest.org/en/latest/)
- [Python unittest Dokumentation](https://docs.python.org/3/library/unittest.html)