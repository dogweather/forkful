---
title:                "Fehlerbehandlung"
date:                  2024-01-26T00:58:17.232647-07:00
model:                 gpt-4-1106-preview
simple_title:         "Fehlerbehandlung"

category:             "Python"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/handling-errors.md"
---

{{< edit_this_page >}}

## Was & Warum?

Fehlerbehandlung in Python (oder jeder anderen Programmiersprache) bedeutet, mit dem Unerwarteten zu rechnen – es ist die Kunst, anmutig zu handhaben, wenn etwas in Ihrem Code schiefgeht. Wir tun dies, um Abstürze zu verhindern, Benutzer zu leiten und unsere Programme robust und zuverlässig zu machen.

## Wie geht das:

``` Python
# Grundlegender try-except-Block
try:
    # riskanter Code
    zahl = int(input("Geben Sie eine Zahl ein: "))
except ValueError:
    # Fehler behandeln
    print("Das ist keine Zahl!")

# Mehrere Ausnahmen angeben
try:
    # Code, der verschiedene Ausnahmen auslösen könnte
    ergebnis = 10 / int(input("Geben Sie einen Divisor ein: "))
except ZeroDivisionError:
    print("Ups! Man kann nicht durch null teilen.")
except ValueError:
    print("Ich brauche eine Zahl, Kumpel.")

# Verwendung von else und finally
try:
    zahl = int(input("Geben Sie eine Zahl zum Quadrat ein: "))
except ValueError:
    print("Ich sagte eine Zahl!")
else:
    # keine Fehler aufgetreten
    print("Ihre Zahl im Quadrat ist:", zahl**2)
finally:
    # wird immer ausgeführt
    print("Danke fürs Ausprobieren!")
```

Beispielausgabe bei Eingabe einer ungültigen Zahl für den ersten Block:
```
Geben Sie eine Zahl ein: hallo
Das ist keine Zahl!
```

## Tiefgehender Einblick

Seit den Anfängen der Programmierung ist die Fehlerbehandlung von entscheidender Bedeutung. Frühere Ansätze waren rudimentär, wie das Prüfen von Bedingungen vor jeder riskanten Operation. Python’s `try-except`-Syntax stammt aus einem Erbe der Ausnahmebehandlung in älteren Sprachen wie C++ und Java, welches den Prozess vereinfacht.

Wenn Sie einen Codeblock `try`, beobachtet Python auf alle Ausnahmen. Tritt ein Fehler auf, fängt der `except`-Block diesen ab. Sie können genau spezifizieren, welche Ausnahmen Sie fangen, oder Sie fangen sie alle mit einem einfachen `except`. Allerdings ist der spezifische Ansatz zuerst der bessere – er ist präzise, kein Auffangnetz für alles.

`else` und `finally` sind Extras in diesem Konzept. Der `else`-Block wird ausgeführt, wenn im try-Block keine Fehler auftreten. `finally` ist der zuverlässige Kumpel, der läuft, egal was passiert – denken Sie an Aufräumarbeiten.

Alternativen? Die gibt es sicher. Einige Sprachen verwenden Rückgabecodes anstelle von Ausnahmen. Sie könnten auch `with`-Anweisungen für die Ressourcenverwaltung oder `assertions` antreffen, die beim Entwickeln Bedingungen prüfen. Aber wenn wir über solide Fehlerbehandlungsstrategien sprechen, sticht das try-catch-Modell durch seine Lesbarkeit und Struktur hervor.

## Siehe auch

Hier sind einige gute zusätzliche Ressourcen, um noch tiefer einzutauchen:

- Pythons offizielle Dokumentation zu Fehlern und Ausnahmen: [Python Docs – Errors and Exceptions](https://docs.python.org/3/tutorial/errors.html)
- Real Pythons Leitfaden zum Thema: [Real Python - The try/except/else/finally block](https://realpython.com/python-exceptions/)
- Eine durchdachte Diskussion über bewährte Methoden der Fehlerbehandlung: [Stack Overflow – How do I properly ignore exceptions?](https://stackoverflow.com/questions/4990718/about-catching-any-exception)
