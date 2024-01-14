---
title:                "Fish Shell: Tests schreiben"
programming_language: "Fish Shell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Warum sollten wir uns mit dem Schreiben von Tests beschäftigen? Nun, Tests sind ein wichtiger Bestandteil der Softwareentwicklung, die uns dabei helfen, sicherzustellen, dass unser Code korrekt funktioniert und keine unerwarteten Fehler aufweist. Mit Tests können wir auch sicherstellen, dass unser Code weiterhin korrekt funktioniert, auch wenn wir Änderungen an unserem Code vornehmen.

## Wie geht das?

Das Schreiben von Tests in der Fish Shell ist sehr einfach und unkompliziert. Wir erstellen einfach eine Testdatei mit der Erweiterung```.fish``` und schreiben unsere Tests darin. Hier ist ein Beispiel, wie unsere Testdatei für eine Funktion namens "double" aussehen könnte:

```
Fish Shell - Test für "double"

function double -d "Double the given number"
    set -l result $argv[1] * 2
    echo $result
end

function test_double
    assert_equal 4 (double 2)
    assert_equal 100 (double 50)
    assert_equal 0 (double 0)
end
```

In diesem Beispiel haben wir eine Testfunktion namens "double", die eine gegebene Zahl verdoppelt, und wir haben auch eine separate Testfunktion erstellt, um sicherzustellen, dass unsere "double" Funktion korrekt arbeitet. Wir können unsere Tests ausführen, indem wir einfach in unserem Terminal die Befehle ```fish``` und ```source path/to/test/file.fish``` eingeben. Wir sollten dann sehen, dass alle Tests erfolgreich bestanden werden.

## Tiefer Einblick

Das Schreiben von Tests ist nicht nur hilfreich, um Fehler in unserem Code zu erkennen, sondern es kann uns auch dabei unterstützen, besseren und sichereren Code zu schreiben. Indem wir unsere Funktionen mit Tests überprüfen, zwingen wir uns, uns auf die Funktion als Ganzes zu konzentrieren und potenzielle Randfälle zu berücksichtigen. Beim Schreiben von Tests sollten wir auch darauf achten, möglichst unabhängige Tests zu schreiben, um sicherzustellen, dass unsere Funktionen nicht von anderen Abhängigkeiten beeinflusst werden.

## Siehe auch

- [Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html)
- [Überblick über das Testsystem in der Fish Shell](https://fishshell.com/docs/current/scripting.html#testing)