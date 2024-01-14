---
title:                "Elixir: Analyse von HTML"
simple_title:         "Analyse von HTML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## Warum

Das Parsen von HTML ist ein wichtiges Konzept in der Webentwicklung. Es ermöglicht uns, die in HTML geschriebenen Daten in eine strukturierte und lesbare Form umzuwandeln, die wir für unsere Programme nutzen können. In diesem Blog-Beitrag werfen wir einen Blick auf das Parsen von HTML mit Hilfe von Elixir.

## Wie man es macht

Um HTML mit Elixir zu parsen, verwenden wir die beliebte Bibliothek "Floki". Zuerst müssen wir diese Bibliothek in unserem Code importieren:

```Elixir
import Floki
```

Dann können wir eine Webanfrage senden und die HTML-Antwort parsen:

```Elixir
response = HTTPoison.get!("www.example.com")
parsed_html = Floki.parse(response.body)
```

Wir können dann die geparsten Daten durchsuchen und die benötigten Informationen extrahieren:

```Elixir
Floki.find(parsed_html, "h1")
# output: ["Welcome to my website"]
```

## Tiefergehende Informationen

Das Parsen von HTML mit Elixir ist effizient und schnell. Floki verwendet den Begriff "DOM", um die Struktur des geparsten HTML-Dokuments zu beschreiben. DOM steht für Dokumentobjektmodell und stellt das HTML-Dokument als eine Sammlung von Knoten dar, die miteinander verknüpft sind. Floki ermöglicht es uns, auf diese Knoten zuzugreifen und sie zu manipulieren.

Es ist wichtig zu beachten, dass die Struktur des DOM von verschiedenen Faktoren wie Schachtelung, Einrückungen und Leerzeichen beeinflusst werden kann. Es ist daher immer ratsam, verschiedene Möglichkeiten der Datenextraktion zu testen, um sicherzustellen, dass wir die korrekten Informationen erhalten.

## Siehe auch

- [Offizielle Floki-Dokumentation](https://hexdocs.pm/floki/readme.html)
- [Elixir School - Parsing HTML with Floki](https://elixirschool.com/en/lessons/advanced/pattern-matching/#parsing-html-with-floki)
- [Elixir Forum - How to use Floki like a Pro](https://elixirforum.com/t/how-to-use-floki-like-a-pro-advanced-elixir-tips-tricks/27423)