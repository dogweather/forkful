---
date: 2024-01-26 04:29:30.299372-07:00
description: "Come fare: Elixir non include l'analisi XML nella sua libreria standard.\
  \ SweetXML \xE8 una scelta popolare. Ecco come utilizzarlo."
lastmod: '2024-03-13T22:44:43.108847-06:00'
model: gpt-4-0125-preview
summary: Elixir non include l'analisi XML nella sua libreria standard.
title: Lavorare con XML
weight: 40
---

## Come fare:
Elixir non include l'analisi XML nella sua libreria standard. SweetXML è una scelta popolare. Ecco come utilizzarlo:

```elixir
# Aggiungi SweetXML alle tue dipendenze in mix.exs
{:sweet_xml, "~> 0.6"}

# Nel tuo codice
import SweetXml

xml = """
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Promemoria</heading>
  <body>Non dimenticarmi questo fine settimana!</body>
</note>
"""

# Analizza XML
note = xml |> xpath(~x"//note")
to = xml |> xpath(~x"//note/to" |> inner_text())
IO.puts to # Output: Tove
```

## Approfondimento
XML, o Extensible Markup Language, esiste dalla fine degli anni '90. È verboso ma strutturato—ideale per lo scambio di dati complessi. Mentre la popolarità di JSON è aumentata per la sua semplicità, XML rimane radicato in molti sistemi aziendali e finanziari per la sua espressività e schemi standardizzati.

Le alternative includono:
- JSON per uno scambio di dati più leggero e meno verboso.
- Protobuf o Thrift per la comunicazione di dati serializzati binari, particolarmente per i sistemi interni.

Sotto il cofano, le librerie XML per Elixir sfruttano la libreria :xmerl di Erlang per l'analisi, che fornisce un supporto robusto ma può essere meno intuitiva rispetto ad approcci più moderni. Man mano che Elixir evolve, le librerie guidate dalla comunità come SweetXML avvolgono queste con una sintassi più elixiresca, rendendo le manipolazioni XML più accessibili.

## Vedi Anche:
- SweetXML su Hex: https://hex.pm/packages/sweet_xml
- Il punto di vista di Elixir sull'analisi XML: https://elixir-lang.org/getting-started/mix-otp/dependencies-and-umbrella-projects.html
- Documentazione xmerl per la gestione sottostante dell'XML: http://erlang.org/doc/apps/xmerl/index.html
