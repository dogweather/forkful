---
title:                "Eine HTTP-Anforderung senden"
html_title:           "Bash: Eine HTTP-Anforderung senden"
simple_title:         "Eine HTTP-Anforderung senden"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Senden einer HTTP-Anfrage ist der Vorgang der Kommunikation mit einem Server, um Informationen abzurufen oder zu verändern. Programmierer nutzen diese Technik, um datenintensive Aktionen wie Erstellen, Lesen, Aktualisieren und Löschen (CRUD) von Daten zu automatisieren.

## So geht's:

In Elixir benutzen wir die Bibliothek HTTPoison. Hier ist ein Beispiel, wie man damit eine HTTP-Anfrage sendet:

```Elixir
{:ok, %HTTPoison.Response{body: body}} = HTTPoison.get("http://example.com")
IO.puts body
```

Und so sieht die Ausgabe aus:

```Elixir
"<html>Beispiel-HTML-Inhalt</html>"
```

## Vertiefung:

Senden von HTTP Anfragen ist nicht neu. Das HTTP-Protokoll wurde Ende der 1980er Jahre entwickelt, um eine standardisierte Methode für die Kommunikation zwischen Clients und Servern zu schaffen. Es gibt viele Alternativen zum Senden von HTTP-Anfragen. Elixir unterstützt auch andere Bibliotheken wie Tesla und HTTPotion.

Die Implementierung von HTTPoison in Elixir basiert auf der erlang-typischen Concurrency- und Fehlerbehandlungsphilosophie. Die Bibliothek nutzt den Hackney HTTP Client, welcher wiederum die erlang-native Bibliothek gen_tcp verwendet.

## Siehe auch:

- [HTTPoison Dokumentation](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Erlang gen_tcp Dokumentation](http://erlang.org/doc/man/gen_tcp.html)
- [Tesla Dokumentation](https://hexdocs.pm/tesla/readme.html)
- [HTTPotion Dokumentation](https://hexdocs.pm/httpotion/readme.html)