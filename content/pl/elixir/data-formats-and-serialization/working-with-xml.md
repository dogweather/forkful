---
date: 2024-01-26 04:29:59.001384-07:00
description: "Jak to zrobi\u0107: Elixir standardowo nie zawiera analizy XML w swojej\
  \ bibliotece standardowej. Popularnym wyborem jest SweetXML. Oto jak z niego korzysta\u0107\
  ."
lastmod: '2024-03-13T22:44:35.069695-06:00'
model: gpt-4-0125-preview
summary: Elixir standardowo nie zawiera analizy XML w swojej bibliotece standardowej.
title: Praca z XML
weight: 40
---

## Jak to zrobić:
Elixir standardowo nie zawiera analizy XML w swojej bibliotece standardowej. Popularnym wyborem jest SweetXML. Oto jak z niego korzystać:

```elixir
# Dodaj SweetXML do swoich zależności w mix.exs
{:sweet_xml, "~> 0.6"}

# W swoim kodzie
import SweetXml

xml = """
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Przypomnienie</heading>
  <body>Nie zapomnij o mnie w ten weekend!</body>
</note>
"""

# Analiza XML
note = xml |> xpath(~x"//note")
to = xml |> xpath(~x"//note/to" |> inner_text())
IO.puts to # Wynik: Tove
```

## Dogłębna analiza
XML, czyli Rozszerzalny Język Znaczników, istnieje od końca lat 90. Jest rozwlekły, ale strukturalny — idealny do złożonej wymiany danych. Pomimo że popularność JSON wzrosła ze względu na jego prostotę, XML pozostaje zakorzeniony w wielu systemach korporacyjnych i finansowych ze względu na swoją ekspresyjność i standaryzowane schematy.

Alternatywy to:
- JSON dla lżejszej, mniej rozwlekłej wymiany danych.
- Protobuf lub Thrift dla binarnie serializowanej komunikacji danych, szczególnie w systemach wewnętrznych.

Pod maską, biblioteki XML dla Elixira wykorzystują bibliotekę :xmerl z Erlanga do analizy, która zapewnia solidne wsparcie, ale może być mniej intuicyjna niż nowocześniejsze podejścia. W miarę rozwoju Elixira, biblioteki prowadzone przez społeczność, takie jak SweetXML, opakowują te funkcje w bardziej Elixir-ową składnię, ułatwiając manipulacje XML.

## Zobacz również:
- SweetXML na Hex: https://hex.pm/packages/sweet_xml
- Podejście Elixira do analizowania XML: https://elixir-lang.org/getting-started/mix-otp/dependencies-and-umbrella-projects.html
- Dokumentacja xmerl na temat obsługi XML: http://erlang.org/doc/apps/xmerl/index.html
