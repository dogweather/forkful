---
title:    "Bash: Tests schreiben"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/bash/writing-tests.md"
---

{{< edit_this_page >}}

# Warum

Tests zu schreiben ist ein wichtiger Teil des Programmierens. Sie helfen uns, unsere Codes zu überprüfen und zu verbessern, bevor wir sie in die Produktion schicken. Auch können Tests helfen, mögliche Fehler frühzeitig zu erkennen und zu beheben, was uns Zeit und Mühe ersparen kann. Deshalb ist es wichtig, sich mit dem Thema Tests vertraut zu machen und sie in unsere Entwicklungspraxis zu integrieren.

## Wie geht man vor?

Um Tests zu schreiben, nutzen wir in der Regel sogenannte Testframeworks, wie zum Beispiel **Bash Automated Testing System (BATS)** oder **shUnit2**. Diese ermöglichen es uns, Testfälle zu schreiben und auszuführen, um die Funktionen unserer Codes zu prüfen. Hier ist ein Beispiel, wie wir die BATS-Testframework in einem Bash-Script verwenden können:

```Bash
#!/bin/bash

@test "Funktionsname muss richtig formatiert sein" {
  result="$(./script.sh test)"
  [ "$result" == "Success" ]
}

@test "Wiegen in Kilogramm sollte richtig berechnet werden" {
  result="$(./script.sh weight)"
  [ "$result" == "5 kg" ]
}
```

In diesen Testfällen überprüfen wir, ob die Funktionen "Funktionsname" und "Wiegen" unseres Skripts die erwarteten Ergebnisse liefern. Wir testen sie, indem wir das Skript mit verschiedenen Parametern ausführen und die Ausgabe mit den erwarteten Ergebnissen vergleichen.

## Tiefer tauchen

Tests zu schreiben erfordert ein gutes Verständnis unserer Codes und ihrer Funktionen. Deshalb sollten wir beim Schreiben von Tests auch auf die Einhaltung von Best Practices achten. Zum Beispiel sollten wir sicherstellen, dass unsere Tests unabhängig voneinander und wiederholbar sind. Auch ist es wichtig, verschiedene Szenarien abzudecken und Grenzfälle zu testen, um sicherzustellen, dass unsere Codes robust und fehlerfrei sind.

Ein weiterer wichtiger Aspekt ist es, unsere Tests regelmäßig auszuführen und die Ergebnisse zu überprüfen. Wenn wir Änderungen an unserem Code vornehmen, müssen wir auch unsere Tests anpassen oder neue hinzufügen, um sicherzustellen, dass alle Funktionen weiterhin ordnungsgemäß arbeiten.

## Siehe auch

- [Bash Automated Testing System (BATS)](https://github.com/bats-core/bats-core)
- [shUnit2](https://github.com/kward/shunit2)
- [Test-Driven Development (TDD) in Bash](https://blog.dcycle.com/blog/2014-07-13/test-driven-development-tdd-bash/)