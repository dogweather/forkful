---
title:                "Arbeiten mit XML"
date:                  2024-01-26T04:29:21.076087-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit XML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/working-with-xml.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Arbeit mit XML in Elixir bedeutet das Parsen, Erstellen und Manipulieren von XML-Daten. Programmierer beschäftigen sich mit XML, weil es weit verbreitet in Webdiensten, Konfigurationsdateien und älteren Systemen ist.

## Wie:
Elixir beinhaltet keine XML-Parsing-Funktionen in seiner Standardbibliothek. SweetXML ist eine beliebte Wahl. So verwenden Sie es:

```elixir
# Füge SweetXML zu deinen Abhängigkeiten in mix.exs hinzu
{:sweet_xml, "~> 0.6"}

# In deinem Code
importiere SweetXml

xml = """
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Erinnerung</heading>
  <body>Vergiss mich dieses Wochenende nicht!</body>
</note>
"""

# Parse XML
note = xml |> xpath(~x"//note")
an = xml |> xpath(~x"//note/to" |> inner_text())
IO.puts an # Ausgabe: Tove
```

## Tiefergehend
XML oder Extensible Markup Language existiert seit den späten 90ern. Es ist umfangreich, aber strukturiert – ideal für komplexen Datenaustausch. Obwohl die Beliebtheit von JSON wegen seiner Einfachheit in die Höhe schnellte, bleibt XML in vielen Unternehmens- und Finanzsystemen wegen seiner Ausdrucksstärke und standardisierten Schemas verankert.

Alternativen beinhalten:
- JSON für einen leichteren, weniger umfangreichen Datenaustausch.
- Protobuf oder Thrift für binär serialisierte Datenkommunikation, besonders für interne Systeme.

Unter der Oberfläche nutzen XML-Bibliotheken für Elixir Erlangs :xmerl-Bibliothek zum Parsen, die robuste Unterstützung bietet, aber weniger intuitiv als modernere Ansätze sein kann. Während Elixir sich entwickelt, umhüllen communitygetriebene Bibliotheken wie SweetXML diese mit einer eher Elixir-ähnlichen Syntax, was XML-Manipulationen zugänglicher macht.

## Siehe auch:
- SweetXML auf Hex: https://hex.pm/packages/sweet_xml
- Elixirs Herangehensweise an das XML-Parsing: https://elixir-lang.org/getting-started/mix-otp/dependencies-and-umbrella-projects.html
- xmerl-Dokumentation für die zugrundeliegende XML-Verarbeitung: http://erlang.org/doc/apps/xmerl/index.html