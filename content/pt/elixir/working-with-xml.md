---
title:                "Trabalhando com XML"
date:                  2024-01-26T04:29:49.172721-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com XML"

category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/working-with-xml.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Trabalhar com XML em Elixir significa analisar, criar e manipular dados XML. Programadores lidam com XML porque ele é amplamente utilizado em serviços web, arquivos de configuração e sistemas legados.

## Como fazer:
Elixir não inclui análise de XML em sua biblioteca padrão. SweetXML é uma escolha popular. Aqui está como usá-lo:

```elixir
# Adicione o SweetXML às suas dependências em mix.exs
{:sweet_xml, "~> 0.6"}

# No seu código
import SweetXml

xml = """
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Lembrete</heading>
  <body>Não me esqueça este fim de semana!</body>
</note>
"""

# Analisar XML
note = xml |> xpath(~x"//note")
to = xml |> xpath(~x"//note/to" |> inner_text())
IO.puts to # Saída: Tove
```

## Aprofundando
XML, ou Linguagem de Marcação Extensível, existe desde o final dos anos 90. É verboso, mas estruturado — ideal para intercâmbio de dados complexos. Enquanto a popularidade do JSON disparou pela sua simplicidade, o XML permanece enraizado em muitos sistemas empresariais e financeiros por sua expressividade e esquemas padronizados.

Alternativas incluem:
- JSON para troca de dados mais leve e menos verbosa.
- Protobuf ou Thrift para comunicação de dados serializados binários, particularmente para sistemas internos.

Por baixo do capô, as bibliotecas XML para Elixir aproveitam a biblioteca :xmerl da Erlang para análise, que fornece suporte robusto, mas pode ser menos intuitiva do que abordagens mais modernas. À medida que Elixir evolui, bibliotecas impulsionadas pela comunidade como SweetXML envolvem essas com uma sintaxe mais Elixir-esque, tornando manipulações XML mais acessíveis.

## Veja também:
- SweetXML no Hex: https://hex.pm/packages/sweet_xml
- Abordagem do Elixir para análise de XML: https://elixir-lang.org/getting-started/mix-otp/dependencies-and-umbrella-projects.html
- Documentação do xmerl para o tratamento subjacente do XML: http://erlang.org/doc/apps/xmerl/index.html
