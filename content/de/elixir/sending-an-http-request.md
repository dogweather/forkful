---
title:                "Einen HTTP-Request senden"
date:                  2024-01-20T17:59:17.117840-07:00
model:                 gpt-4-1106-preview
simple_title:         "Einen HTTP-Request senden"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTTP-Anfragen senden bedeutet, dass Daten über das Internet von einem Client zu einem Server übermittelt werden. Programmierer nutzen dies, um Informationen abzurufen, Daten zu senden oder Dienste von APIs zu nutzen.

## So geht's:
Installiere HTTPoison mit `mix deps.get`:

```elixir
defp deps do
  [{:httpoison, "~> 1.8"}]
end
```

Einfache GET-Anfrage:

```elixir
HTTPoison.start()

case HTTPoison.get("https://jsonplaceholder.typicode.com/posts/1") do
  {:ok, response} ->
    IO.inspect(response)
  {:error, reason} ->
    IO.inspect(reason)
end
```

Erwartete Ausgabe:

```elixir
%HTTPoison.Response{
  body: "{ ... }",
  status_code: 200,
  ...
}
```

Eine POST-Anfrage mit JSON-Körper:

```elixir
headers = [{"Content-Type", "application/json"}]
body = Jason.encode!(%{title: "Elixir", body: "Rocks!", userId: 1})

HTTPoison.post("https://jsonplaceholder.typicode.com/posts", body, headers)
```
## Tiefgang
HTTP-Anfragen sind der Dreh- und Angelpunkt moderner Web-Anwendungen. Mit dem Aufkommen von REST-APIs und Microservices, spielt HTTP-Kommunikation eine größere Rolle. `HTTPoison` basiert auf `hackney`, einem erprobten HTTP-Client in Erlang.

Alternative Bibliotheken sind beispielsweise `Tesla` oder `Finch`. Im Kern nutzen sie alle das Erlang `:httpc`-Modul oder angepasste Adapter, um HTTP-Anfragen zu verarbeiten. Implementierungen können asynchron oder synchron sein und viele unterstützen Middlewaren und Plug-ins für erweiterte Funktionalitäten.

## Siehe Auch
- Offizielle Dokumentation von HTTPoison: https://hexdocs.pm/httpoison
- Elixir's GitHub-Seite: https://github.com/elixir-lang/elixir
- Erlang's `:httpc` Dokumentation: http://erlang.org/doc/man/httpc.html
- JSON Handling mit Jason: https://hexdocs.pm/jason/readme.html
- REST-API-Grundlagen: https://restfulapi.net/
