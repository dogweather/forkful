---
date: 2024-01-26 01:01:26.713624-07:00
description: "Logging in der Softwareentwicklung ist die Technik, Ereignisse aufzuzeichnen,\
  \ die w\xE4hrend des Betriebs eines Programms auftreten, typischerweise in einer\u2026"
lastmod: 2024-02-19 22:05:12.518019
model: gpt-4-1106-preview
summary: "Logging in der Softwareentwicklung ist die Technik, Ereignisse aufzuzeichnen,\
  \ die w\xE4hrend des Betriebs eines Programms auftreten, typischerweise in einer\u2026"
title: Protokollierung
---

{{< edit_this_page >}}

## Was & Warum?
Logging in der Softwareentwicklung ist die Technik, Ereignisse aufzuzeichnen, die während des Betriebs eines Programms auftreten, typischerweise in einer Datei oder in einem externen System. Programmierer machen das, um Einblicke in das Verhalten der Software zu gewinnen, Probleme zu diagnostizieren und einen Betriebsverlauf zu dokumentieren, der für das Debugging und die Überwachung der Anwendungsgesundheit entscheidend ist.

## Wie geht das:
In Elixir ist der primäre Weg, Informationen zu loggen, durch das eingebaute `Logger`-Modul. So können Sie es verwenden:

```elixir
defmodule MyApplication do
  require Logger

  def do_something_important(param) do
    Logger.info("Starte wichtigen Prozess mit Parameter: #{param}")

    # Simuliere die Erledigung der Arbeit
    :timer.sleep(1000)

    Logger.debug("Prozess abgeschlossen.")
  rescue
    error -> Logger.error("Ein Fehler ist aufgetreten: #{inspect(error)}")
  end
end

# Um Ihre Logs zu sehen, rufen Sie einfach die Funktion auf:
MyApplication.do_something_important("MeinParameter")
```

Dieser einfache Codeausschnitt zeigt, wie man auf verschiedenen Ebenen loggt (`info`, `debug` und `error`). Wenn Sie dies ausführen, werden Sie die Debug-Meldung nicht sehen, es sei denn, Sie konfigurieren das Logger-Level auf `:debug`. Standardmäßig filtert Elixirs Logger Log-Nachrichten unterhalb von `:info` heraus.

Eine beispielhafte Ausgabe auf dem `:info`-Level könnte so aussehen:
```
14:32:40.123 [info]  Starte wichtigen Prozess mit Parameter: MeinParameter
14:32:41.126 [error] Ein Fehler ist aufgetreten: %RuntimeError{message: "Laufzeitfehler"}
```

## Vertiefung:
Elixirs `Logger` ist ein eingebautes Werkzeug, das seit den Anfangstagen der Sprache Teil davon ist. Es ist beeinflusst von den Log-Systemen anderer BEAM-Sprachen wie Erlang. Der Logger bietet verschiedene Log-Level – `:debug`, `:info`, `:warn` und `:error` – und er ist erweiterbar, sodass verschiedene Backends für die Handhabung von Log-Nachrichten eingebunden werden können.

Eine Alternative zum eingebauten Logger für komplexere Szenarien ist die Verwendung von Logging-Bibliotheken wie `Logstash` oder `Sentry` für Elixir, die zusätzliche Funktionen wie Fehlerverfolgung und Aggregation in einem visuelleren Format bieten können. Für lokale Entwicklung verlassen sich Elixir-Entwickler oft auf die eingebaute Logger-Funktionalität aufgrund ihrer Einfachheit und Integration mit der BEAM VM.

Unter der Haube bietet das Logger-Modul asynchrones und synchrones Logging. Asynchrones Logging, welches der Standard ist, blockiert die Ausführung Ihrer Anwendung nicht beim Loggen der Nachrichten. Dies stellt sicher, dass das Logging die Leistung nicht negativ beeinflusst. Jedoch kann synchrones Logging für Fälle aktiviert werden, in denen Sie garantieren müssen, dass Nachrichten in der Reihenfolge geloggt werden, in der sie gesendet wurden.

Die Logger-Konfiguration kann in der `config/config.exs`-Datei einer Elixir-Anwendung angepasst werden, wo Sie das Logging-Level, Format, Metadaten und mehr einstellen können. Denken Sie immer daran, Ihre Logging-Levels und Ausgaben für verschiedene Umgebungen anzupassen; Sie möchten nicht, dass ausführliche Debug-Logs Ihre Produktionssysteme überfluten.

## Siehe auch:
- Die offizielle Elixir-Logger-Dokumentation: https://hexdocs.pm/logger/Logger.html
- Ein Blogpost über Elixir-Logging-Best-Practices: https://blog.appsignal.com/2020/05/06/elixir-logging-tips-and-tricks.html
- Sentry für Elixir auf Hex: https://hex.pm/packages/sentry
- Elixir School's Lektion über Logger: https://elixirschool.com/en/lessons/specifics/debugging/#logging
