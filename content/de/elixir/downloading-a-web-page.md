---
title:                "Elixir: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum

Elixir ist eine beliebte Skriptsprache, die sich durch ihre einfache Syntax und Skalierbarkeit auszeichnet. Wenn du dich für Webentwicklung interessierst und mit Elixir arbeitest, möchtest du vielleicht auch Webseiten herunterladen und darauf zugreifen. In diesem Blogartikel erfährst du, wie du das mit Elixir tun kannst.

## Wie geht's

Um eine Webseite mit Elixir herunterzuladen, benötigen wir das HTTPoison-Paket. Installiere es zuerst mit dem Befehl `mix deps.get`. Danach können wir mit dem Herunterladen der Webseite beginnen. Der folgende Code zeigt, wie du eine Webseite mit Elixir herunterladen und als String ausgeben kannst.

```Elixir
defmodule Webpage do
  use HTTPoison.Base

  def process_response(response, {:ok, body}) do
    body
  end
end

response = HTTPoison.get!("http://www.deine-webseite.de")
IO.puts response.body
```

Wenn du diesen Code ausführst, solltest du den HTML-Code der Webseite in deiner Konsole sehen.

## Tiefer eintauchen

Das Herunterladen einer Webseite mit Elixir kann noch weiter optimiert werden, indem du zum Beispiel den User-Agent änderst oder Cookies verwendest. Hier ist ein Beispielcode, der das Herunterladen einer Webseite mit einem angepassten User-Agent und dem Hinzufügen von Cookies zeigt.

```Elixir
defmodule Webpage do
  use HTTPoison.Base

  def process_response(response, {:ok, body}) do
    body
  end
end

headers = [{"User-Agent", "Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2490.71 Safari/537.36"}]
cookies = [{"Cookie", "session_id=123456789"}]

response = HTTPoison.get!("http://www.deine-webseite.de", headers, cookies)
IO.puts response.body
```

Wie du siehst, ist es mit Elixir möglich, beliebige HTTP-Header und Cookies hinzuzufügen, um die Anfrage an die Webseite zu personalisieren.

## Siehe auch

- [HTTPoison Dokumentation](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Elixir Website](https://elixir-lang.org/)
- [Elixir Forum](https://elixirforum.com/)