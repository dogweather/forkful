---
title:                "Fish Shell: Ausgabe von Debugging-Informationen"
simple_title:         "Ausgabe von Debugging-Informationen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

# Warum

Das Drucken von Debug-Ausgaben ist eine nützliche Technik, um während des Programmierens Probleme zu identifizieren und zu beheben. Durch das Anzeigen von Ausgaben in Ihrem Code können Sie verfolgen, was Ihr Programm tut und welche Variablen aktualisiert werden. Auf diese Weise können Sie Fehler effizienter beheben und Ihre Code-Qualität verbessern.

# Wie geht's?

Mit Fish Shell können Sie Debug-Ausgaben auf einfache Weise erstellen. Alles, was Sie tun müssen, ist, den Befehl "echo" zu verwenden, gefolgt von der zu druckenden Nachricht oder Variablen. Zum Beispiel:

```Fish Shell
set name Peter
echo "Hello, $name!"
```

Dieser Code wird die Ausgabe "Hello, Peter!" erzeugen. Beachten Sie, dass wir die Variable "name" mit dem Präfix "$" verwenden, um ihren Wert in der Nachricht anzuzeigen. Auf diese Weise können wir die Werte von Variablen in unseren Debug-Ausgaben überwachen.

# Tiefer Einblick

Es gibt einige Optionen, die Sie beim Drucken von Debug-Ausgaben mit Fish Shell berücksichtigen sollten. Zum Beispiel können Sie mit dem Flag "-n" am Ende des Befehls "echo" Leerzeichen zwischen den Argumenten entfernen. Sie können auch die Ausgabe von mehreren Befehlen in derselben Zeile kombinieren, indem Sie sie mit "&&" verketten.

Eine weitere nützliche Technik ist, die Ausgabe in eine Datei umzuleiten, statt sie in der Konsole anzuzeigen. Dies kann hilfreich sein, um große Mengen an Debug-Ausgaben zu erfassen und später zu überprüfen. Zum Beispiel:

```Fish Shell
echo "Debug-Ausgabe" >> debug.log
```

Diese Zeile fügt die Nachricht "Debug-Ausgabe" am Ende der Datei "debug.log" hinzu. Sie können auch den Befehl "tee" verwenden, um die Ausgabe sowohl in die Konsole als auch in eine Datei zu leiten.

# Siehe auch

- Fish Shell Dokumentation: https://fishshell.com/docs/current/
- Solving Problems with Fish Shell's Echo Command: https://dev.to/erikaheidi/solving-problems-with-fish-shell-s-echo-command-5695
- Mastering Debugging in Fish Shell: https://medium.com/@peterpme/mastering-debugging-in-fish-shell-435befb1bbf5