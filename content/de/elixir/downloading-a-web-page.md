---
title:                "Webseite herunterladen"
html_title:           "Elixir: Webseite herunterladen"
simple_title:         "Webseite herunterladen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Herunterladen einer Webseite beinhaltet das Abrufen von Ressourcen wie Text, Bilder und Videos aus dem Internet. Programmierer tun dies, um Daten zu extrahieren, analysieren oder anzeigen zu können.

# Wie geht das?
Das Herunterladen einer Webseite in Elixir ist sehr einfach. Verwenden Sie einfach den HTTP Client ```Elixir HTTPoison``` und rufen Sie die gewünschte URL auf. Zum Beispiel:
```Elixir
response = HTTPoison.get("https://www.example.com")
```
Der Inhalt der Webseite kann dann mit ```response.body``` abgerufen werden und weitere Analyse oder Verarbeitung kann durchgeführt werden.

# Tiefere Einblicke
Das Herunterladen von Webseiten ist eine grundlegende Funktion in der Webentwicklung und wird von vielen anderen Sprachen und Frameworks unterstützt. Alternativen zu ```HTTPoison``` sind beispielsweise ```Elixir HTTPotion``` und ```Elixir Finch```. Die Implementierung von ```HTTPoison``` basiert auf der hochperformanten Erlang Library ```hackney``` und bietet eine einfache API für den Einsatz in Elixir Projekten.

# Siehe auch
Für weitere Informationen und Beispiele zur Verwendung von ```HTTPoison```, besuchen Sie die offizielle Dokumentation unter https://hexdocs.pm/httpoison. Weitere Ressourcen zu Webentwicklung in Elixir sind das Elixir Forum unter https://elixirforum.com und der Elixir Podcast unter https://elixiroutlaws.com.