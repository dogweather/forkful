---
title:                "Tests schreiben"
html_title:           "Fish Shell: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

Warum:

Es kann mühsam sein, Tests zu schreiben, aber es hat viele Vorteile. Tests helfen, Fehler im Code frühzeitig zu erkennen und verhindern Regressionen bei zukünftigen Änderungen. Sie ermöglichen auch eine schnellere Fehlerbehebung und verbessern die Code-Qualität insgesamt.

Wie geht's:

```Fish Shell``` enthält ein eingebautes Test-Framework namens ```fish_assert```, das es einfach macht, Tests zu schreiben. Hier ist ein einfaches Beispiel eines Tests, der überprüft, ob die Ausgabe einer Funktion mit der erwarteten Ausgabe übereinstimmt:

```
function double (num)
    return $num * 2
end

fish_assert double(5) -eq 10
```
Dieser Test verwendet die Funktion ```fish_assert```, um zu überprüfen, ob die Ausgabe von ```double(5)``` gleich 10 ist. Wenn dies der Fall ist, gibt der Test erfolgreich zurück. Andernfalls wird eine Fehlermeldung ausgegeben.

Ein weiteres Beispiel ist ein Test mit benutzerdefinierten Nachrichten, um den Testfall zu beschreiben:

```
function is_even (num)
    return $num % 2 == 0
end

fish_assert is_even(4) -eq 1 "Expected 4 to be even, but it was odd."
```
Dieser Test verwendet den zusätzlichen Parameter, um eine Nachricht anzugeben, die angezeigt wird, wenn der Test fehlschlägt. Dies kann hilfreich sein, um den Grund für den Fehler schnell zu identifizieren.

Tiefentauchen:

Es gibt verschiedene Arten von Tests, die in der Praxis verwendet werden können, wie z.B. Unit-Tests, Integrationstests und Funktionstests. Es ist wichtig, dass Tests ausreichend abdeckend sind und verschiedene Szenarien abdecken, um sicherzustellen, dass der Code korrekt funktioniert.

Es ist auch wichtig, dass Tests regelmäßig ausgeführt werden und bei Bedarf aktualisiert werden, um sicherzustellen, dass sie mit den Änderungen im Code Schritt halten. Es ist eine gute Praxis, Tests automatisiert auszuführen, z.B. bei jedem Push in ein Repository oder als Teil eines Continuous Integration-Prozesses.

Siehe auch:

- [Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html#assertions)
- [Einleitung zu Tests mit Fish Shell](https://dev.to/johlepisto/introduction-to-tests-with-fish-shell-1k9h)