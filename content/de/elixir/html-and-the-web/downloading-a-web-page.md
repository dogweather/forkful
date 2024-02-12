---
title:                "Webseite herunterladen"
aliases:
- /de/elixir/downloading-a-web-page/
date:                  2024-01-20T17:44:02.455773-07:00
model:                 gpt-4-1106-preview
simple_title:         "Webseite herunterladen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Herunterladen einer Webseite bedeutet, ihren Inhalt über das Internet abzurufen. Programmierer machen das, um Daten zu sammeln, automatisierte Tests durchzuführen oder Inhalte für die Verarbeitung zu extrahieren.

## So geht's:
Elixir verwendet die HTTPoison-Bibliothek für HTTP-Anfragen. Wir installieren HTTPoison, machen eine Anfrage und verarbeiten die Antwort:

```elixir
# In mix.exs, fügen Sie {:httpoison, "~> 1.8"} zum Abschnitt :deps hinzu.
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end

# Dann führen Sie `mix deps.get` aus, um die Abhängigkeit zu installieren.

# Verwenden Sie HTTPoison, um eine Webseite herunterzuladen:
defmodule PageDownloader do
  def fetch(url) do
    case HTTPoison.get(url) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        {:ok, body}
      {:ok, %HTTPoison.Response{status_code: code}} ->
        {:error, "Unable to fetch page. Status code: #{code}"}
      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, "Error fetching page: #{reason}"}
    end
  end
end

# Beispielaufruf und mögliche Ausgabe:
PageDownloader.fetch("http://example.com")
# => {:ok, "<html>...</html>"}
```

## Deep Dive:
Elixir-Programmierer nutzen oft HTTPoison, das auf Erlangs `hackney`-Bibliothek basiert, um HTTP-Anfragen zu machen. Es gibt Alternativen wie `Tesla`, das Middleware unterstützt, und `HTTPotion`, wobei HTTPoison meistens für seine Einfachheit und Leistungsfähigkeit bevorzugt wird. Beim Herunterladen einer Webseite müssen Dinge wie Antwort-Codes, Weiterleitungen, Timeouts und Fehlerbehandlung berücksichtigt werden.

## Siehe Auch:
- [HTTPoison documentation](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Erlang `hackney` library](https://github.com/benoitc/hackney)
- [Tesla Elixir library](https://github.com/teamon/tesla)
