---
title:                "Herunterladen einer Webseite"
html_title:           "Elixir: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum

Du hast dich wahrscheinlich schon mal gefragt, wie du eine Web-Seite herunterladen und ihre Inhalte auf deinem Computer speichern kannst. Vielleicht möchtest du die Seite offline lesen oder weitere Anpassungen vornehmen. In diesem Artikel werde ich dir zeigen, wie du dieses Problem mit Elixir ganz einfach lösen kannst.

## Wie geht das?

Zuerst müssen wir die HTTPoison Bibliothek in unser Projekt einbinden:

```elixir
def deps do
  [
    {:httpoison, "~> 1.5"}
  ]
end
```

Als nächstes erstellen wir eine Funktion, die die gewünschte URL als Argument nimmt und die Seite herunterlädt:

```elixir
def load_page(url) do
  {:ok, response} = HTTPoison.get(url)
  response.body
end
```

Und schon haben wir den gesamten HTML-Code der Seite in der Variable `body` gespeichert.

Um die Inhalte der Seite zu extrahieren oder weiter zu bearbeiten, können wir zum Beispiel die Floki Bibliothek verwenden. Diese erlaubt uns, HTML-Dokumente auf einfache Weise zu durchsuchen und Daten abzurufen.

## Tiefergehend

Die `load_page` Funktion, die wir zuvor geschrieben haben, kann noch weiter verbessert werden. Zum Beispiel könnten wir eine Fehlerbehandlung hinzufügen, um sicherzustellen, dass der Server antwortet und die Seite tatsächlich verfügbar ist.

Außerdem können wir auch verschiedene Optionen für unsere HTTP-Anfrage angeben, wie zum Beispiel die Verwendung von Cookies oder die Angabe von Headern.

Elixir bietet uns mit seinen reaktiven und funktionalen Eigenschaften die Möglichkeit, flexibel auf solche Situationen zu reagieren und unsere Funktionen effektiv zu gestalten.

## Siehe auch

- Offizielle Website von Elixir: https://elixir-lang.org/de/
- HTTPoison Dokumentation: https://hexdocs.pm/httpoison/HTTPoison.html
- Floki Dokumentation: https://hexdocs.pm/floki/Floki.html