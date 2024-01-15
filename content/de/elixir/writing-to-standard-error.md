---
title:                "Schreiben auf den Standardfehler"
html_title:           "Elixir: Schreiben auf den Standardfehler"
simple_title:         "Schreiben auf den Standardfehler"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Warum

Das Schreiben in das Standardfehlerprotokoll, auch bekannt als stderr, ist eine häufige Praxis bei der Programmierung in Elixir. Es ermöglicht Entwicklern, Fehler und Warnungen während der Ausführung ihres Codes zu verfolgen und zu debuggen.

# Wie es geht

Um in Elixir in das Standardfehlerprotokoll zu schreiben, verwenden Sie einfach die Funktion `IO.puts/2` und geben Sie als ersten Parameter `:stderr` an. Zum Beispiel:

```Elixir
IO.puts(:stderr, "Dies ist eine Fehlermeldung.")
```

Dieser Code wird "Dies ist eine Fehlermeldung." in das stderr-Protokoll schreiben. Es ist auch möglich, Variablen in der Fehlermeldung zu interpolieren, indem man sie in geschweifte Klammern einschließt. Zum Beispiel:

```Elixir
IO.puts(:stderr, "Der Wert ist: #{my_variable}.")
```

Dies würde den Wert der Variablen `my_variable` in die Fehlermeldung einfügen.

# Tiefes Eintauchen

Das stderr-Protokoll ist besonders nützlich bei der Verwendung von Supervisors in Elixir. Wenn ein Fehler in einem überwachten Prozess auftritt, wird dieser in das stderr-Protokoll geschrieben, was es ermöglicht, den Fehler zu erkennen und zu behandeln. Darüber hinaus können Logging-Frameworks wie `logger` auch auf das stderr-Protokoll zugreifen, um Fehler und Warnungen zu loggen.

# Siehe auch

- Die offizielle Elixir-Dokumentation zu stderr: https://hexdocs.pm/elixir/IO.html#puts/2
- Ein Tutorial zum Schreiben in das stderr-Protokoll in Elixir: https://www.tutorialspoint.com/elixir/elixir_error_handling.htm