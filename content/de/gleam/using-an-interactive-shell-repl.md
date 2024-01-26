---
title:                "Nutzung einer interaktiven Shell (REPL)"
date:                  2024-01-26T04:14:05.611101-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nutzung einer interaktiven Shell (REPL)"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Was & Warum?

Ein REPL, kurz für Read-Eval-Print Loop (Lesen-Auswerten-Drucken Schleife), ist ein Programmierwerkzeug, das es ermöglicht, Code interaktiv auszuführen und sofort Ergebnisse zu sehen. Programmierer nutzen es, um zu experimentieren, Fehler zu finden oder eine neue Sprache wie Gleam spontan zu lernen.

## Wie geht das:

Gleam enthält derzeit kein REPL in seiner Standardverteilung. Sie können jedoch mit Gleam-Code experimentieren, indem Sie die existierende Erlang-Shell verwenden, da Gleam in Erlang-Bytecode kompiliert. So geht's:

1. Kompilieren Sie Ihren Gleam-Code nach Erlang.
```plaintext
gleam build
```

2. Starten Sie die Erlang-Shell.
```plaintext
erl -pa ebin
```

3. Rufen Sie Ihre Gleam-Funktionen auf (angenommen, Sie haben ein Modul namens `my_mod` und eine Funktion `my_fun`).
```erlang
my_mod:my_fun().
```

Sie sollten die Ausgabe Ihrer Funktion in der Shell sehen.

## Tieferer Einblick

REPL verkörpert den dynamischen und explorativen Geist vieler funktionaler Programmiersprachen, zurückgehend auf LISP's REPL in den 1960er Jahren. Im Vergleich bieten andere Systeme wie Pythons `ipython` or Rubys `irb` ähnliche Erfahrungen für ihre Gemeinschaften.

Obwohl Gleam noch kein natives REPL hat, bleibt die Nutzung der Erlang-Shell eine clevere Umgehung. Die Fähigkeiten der Erlang-Shell kommen von der BEAM VM, der virtuellen Maschine, die das Erlang-Ökosystem antreibt, zu dem auch Elixir, LFE und Gleam gehören.

Alternativen zu REPLs im Gleam-Ökosystem könnten das Schreiben von Testfällen oder die Nutzung von Online-Compilern und Code-Spielplätzen einschließen, die Gleam unterstützen, um Code-Schnipsel außerhalb eines vollständigen Projektsetups zu testen.

Die Implementierung eines dedizierten Gleam-REPL steht vor allem vor Herausforderungen bezüglich der kompilierten Natur von Gleam und der Laufzeitumgebung von Erlang, wo das heiße Austauschen von Code die Norm ist. Ein zukünftiges Gleam-REPL müsste die statische Typisierung der Sprache mit der dynamischen Ausführungsumgebung, die ein REPL erwartet, in Einklang bringen.

## Siehe auch

- Gleams offizielle Dokumentation: https://gleam.run/book/
- Erlangs Shell-Dokumentation: http://erlang.org/doc/man/erl.html
- Ein Online-Gleam-Compiler-Spielplatz: https://gleam.run/compiler/