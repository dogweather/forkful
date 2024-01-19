---
title:                "HTML parsen"
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/parsing-html.md"
---

{{< edit_this_page >}}

# Parsen von HTML mit Elixir

## Was & Warum?

HTML-Parsen ist ein Prozess, der die Struktur von HTML-Dokumenten analysiert und sie in verwertbarer Form wiedergibt. Dieses Verfahren ist nützlich, um Informationen aus Webseiten zu gewinnen, Webseiten zu crawlen oder um Webinhalte zu modularisieren und zu transformieren.

## So geht's:

Mit Elixir geht das ganz einfach. Benutzen wir als Beispiel die beliebte Bibliothek `Floki`.

```elixir
defdeps do
    [
      {:floki, "~> 0.30"}
    ]
  end
```

Laden Sie einfach ein HTML-Dokument und benutzen Sie `find` um Elemente auszuwählen.

```elixir 
html = "<div class='ololo'><h2>Hello World</h2></div>"
{"<h2>", _, _} = Floki.find(html, ".ololo h2")
IO.puts content
```

Die Ausgabe wird so aussehen:

``` 
Hello World
``` 

## Tiefgang:

Historisch gesehen wurden HTML-Parsings oft manuell oder mit weniger leistungsfähigen Tools durchgeführt, aber mit dem Einzug funktionaler Programmiersprachen wie Elixir und Libraries wie `Floki` sind wir in der Lage, diese Aufgaben mit größerer Effizienz und Präzision zu bewältigen. 

Alternativ zur Elixir-Library `Floki` können Sie auch `Meeseeks` oder `Sweet_xml` verwenden, die jeweils andere parsing-Strategien anbieten. `Floki` beruht auf ercxml und css Selektoren, während `Meeseeks` auf Rustler und `Sweet_xml` auf Xmerl_xpath aufbaut.

Sich weiter in die Details einzulassen, würde den Rahmen sprengen, aber es ist zu bemerken, dass beim Parsen von HTML immer XPath oder CSS-Selektoren zur Identifizierung und Extraktion spezifischer Elemente zum Einsatz kommen.

## Siehe auch:

Erkunden Sie weitere Techniken und Tools rund um das HTML-Parsing in Elixir durch die folgenden Links:

- [Floki Bibliothek](https://hexdocs.pm/floki/readme.html)
- [Meeseeks Bibliothek](https://hexdocs.pm/meeseeks/readme.html)
- [Sweet_xml Bibliothek](https://hexdocs.pm/sweet_xml/readme.html)
- [Grundlagen von CSS-Selektoren](https://developer.mozilla.org/de/docs/Web/CSS/CSS_Selectors)
- [XPath Tutorial](https://www.w3schools.com/xml/xpath_intro.asp)