---
title:                "Einsatz eines Debuggers"
date:                  2024-01-26T03:48:46.641631-07:00
model:                 gpt-4-0125-preview
simple_title:         "Einsatz eines Debuggers"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/using-a-debugger.md"
---

{{< edit_this_page >}}

## Was & Warum?
Ein Debugger wird im Grunde genommen verwendet, wenn du wie ein Detektiv in deinem Code nach Fehlern suchst und herausfindest, warum Dinge nicht reibungslos laufen. Programmierer tun dies, weil, seien wir ehrlich, Fehler unvermeidlich sind, und sie effizient zu beseitigen bedeutet, deinen Code schneller und zuverlässiger zum Laufen zu bringen.

## Wie:
Gleam stützt sich derzeit auf das Erlang-Ökosystem für Werkzeuge, daher wirst du typischerweise mit Tools wie `rebar3`, `observer` und `debugger` debuggen. Hier ist, wie du tief in das Debugging einsteigen kannst:

```gleam
// In deiner Rebar-Konfiguration stelle sicher, dass du diese Zeilen hast, um Debug-Informationen einzubinden:
{erl_opts, [debug_info]}.

// Starte eine Erlang-Shell mit deiner geladenen App
rebar3 shell

// In der Shell kannst du den Debugger starten
1> debugger:start().
```

Einfach, oder? Die `debugger` GUI erscheint, und du kannst Breakpoints setzen, durch den Code schrittweise durchgehen und Variablen nach Belieben beobachten. Du wirst keinen Gleam-Code direkt sehen, sondern den Erlang-Code, zu dem er kompiliert wird, was immer noch ziemlich hilfreich ist.

## Tiefer Eintauchen
Gleam ist eine junge Sprache, also obwohl es auf den Schultern des Erlang-Ökosystems steht, sind native Gleam-Debugging-Tools noch nicht im Rampenlicht. Das bedeutet, dass wir Erlangs erprobte und wahre Werkzeuge verwenden, und das ist keine schlechte Sache. Erlangs Debugger gibt es seit den 90ern, verfeinert durch Jahre des Ausmerzens lästiger Fehler in Systemen, in denen Zuverlässigkeit entscheidend ist.

Was Alternativen betrifft, so ist Tracing eine mächtige Methode in der BEAM-Welt (das ist die virtuelle Maschine, die Erlang- und Elixier-Code ausführt). Mit `rebar3` kannst du in Werkzeuge wie `recon` eintauchen, um Funktionsaufrufe zu verfolgen und tief in Leistungsprobleme einzudringen.

Der Wechsel zwischen dem Schreiben von Gleam und dem Debuggen in Erlang könnte sich anfühlen, als würdest du deine Gedanken fliegen übersetzen. Aber der Vorteil ist, dass du einen Einblick in die Erlang-Welt erhältst und die Bausteine deiner App in ihrer Laufzeitform verstehst.

## Siehe Auch
Um dein Debugging-Werkzeugset zu erweitern, schau dir an:

- Erlangs Debugger-Dokumentation: [https://erlang.org/doc/apps/debugger/debugger_chapter.html](https://erlang.org/doc/apps/debugger/debugger_chapter.html)
- Die `recon` Bibliothek für Erlang: [https://ferd.github.io/recon/](https://ferd.github.io/recon/)
