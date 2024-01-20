---
title:                "Eine Webseite herunterladen"
html_title:           "Arduino: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Webseiten Herunterladen mit Elixir

## Was & Warum?

Das Herunterladen einer Webseite bedeutet, ihre Inhalte lokal zu speichern. Programmierer tun dies häufig, um Daten zu analysieren oder Offline-Zugriff zu ermöglichen.

## So geht's:

Sie können die HTTPotion-Bibliothek in Elixir verwenden, um eine Website zu holen. Hier ein einfaches Beispiel:

```Elixir
defmodule DownloadWebPage do
  def download(url) do
    HTTPotion.start
    {:ok, response} = HTTPotion.get(url)
    response.body
  end
end

IO.puts DownloadWebPage.download("https://www.example.com")
```

In diesem Code wird die `get`-Methode von HTTPotion aufgerufen, um den Inhalt der angegebenen URL zu erhalten. Das Ergebnis wird dann ausgegeben.

## Deep Dive

Das Herunterladen von Webseiten ist keine neue Praxis. Es reicht bis in die frühen Tage des Internets zurück, als Zugang selten und kostspielig war. Heute wird es bei Web Scrapping, Datamining und für Offline-Zugänge genutzt.

Alternativen zum Herunterladen einer Webseite in Elixir sind andere Bibliotheken wie HTTPoison oder :httpc, die standardmäßig in Erlang/OTP enthalten ist, auf dem Elixir basiert.

Beim Herunterladen von Webseiten ist es wichtig, den "robots.txt" der jeweiligen Webseite zu beachten, um Regeln und Einschränkungen einzuhalten. Zudem sollte man die Anfragen soweit wie möglich limitieren, um die Webseite nicht zu überlasten.

## Siehe Auch

Verwandte Ressourcen zum Thema:

- Elixir's offizielle Dokumentation auf [Elixir School](https://elixirschool.com/de/)
- Erlaubnisse und ethisches Surfen - [Robots.txt](https://developers.google.com/search/docs/advanced/robots/create-robots-txt?visit_id=637672168116277084-3858952796&rd=1)
- Alternative Bibliotheken zur HTTP-Anforderung in Elixir - [HTTPoison auf Github](https://github.com/edgurgel/httpoison) und [:httpc in Erlang/OTP](http://erlang.org/doc/man/httpc.html)