---
title:                "Trabalhando com XML"
date:                  2024-01-26T04:31:21.261414-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com XML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/working-with-xml.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Trabalhar com XML envolve analisar, manipular e gerar documentos XML, que são usados para troca de dados devido ao seu formato estruturado e amplamente difundido. Programadores lidam com XML para interagir com inúmeros sistemas onde o XML é a língua franca dos dados.

## Como fazer:
O Gleam não possui suporte nativo para XML, então usaremos uma biblioteca externa como `gleam_xml`. Primeiro, adicione-a ao seu `gleam.toml`:

```toml
[dependencies]
gleam_xml = "~> 1.0"
```

Agora, analise e crie XML:

```rust
import gleam/xml

// Analisar XML
let doc = xml.parse("<note><to>Tove</to><from>Jani</from></note>")?

// Criar XML
let node = xml.Element(
  "note",
  [],
  [
    xml.Element("to", [], [xml.Text("Tove")]),
    xml.Element("from", [], [xml.Text("Jani")]),
  ]
)
let xml_string = xml.render(node)
```

Saída de exemplo para `xml.render(node)` é:

```xml
<note><to>Tove</to><from>Jani</from></note>
```

## Mergulho Profundo
XML significa eXtensible Markup Language, uma especificação do W3C como irmã da HTML. Está presente desde o final dos anos 90. Para o Gleam, lidar com XML parece um pouco como um passo atrás no tempo. JSON e Protocol Buffers são mais modernos, mas o uso extensivo do XML em sistemas legados e certas indústrias significa que ainda é relevante.

Alternativas como `xmerl` existem no ecossistema Erlang; no entanto, a biblioteca `gleam_xml` oferece uma abordagem mais idiomática para os usuários de Gleam. Ela é construída em cima das bibliotecas Erlang existentes, mas expõe uma API amigável ao Gleam. A abordagem do Gleam para o XML visa simplicidade e segurança, reduzindo o código repetitivo e enfatizando a segurança de tipo.

Em termos de implementação, bibliotecas de XML incluindo `gleam_xml` tipicamente fornecem estruturas semelhantes ao DOM. Isso envolve nós, atributos e elementos aninhados, aproveitando o padrão de correspondência e os modelos de concorrência de Erlang para lidar com documentos potencialmente grandes e complexos.

## Veja Também
- A biblioteca `gleam_xml` no Hex: [https://hex.pm/packages/gleam_xml](https://hex.pm/packages/gleam_xml)
- Padrão oficial XML pelo W3C: [https://www.w3.org/XML/](https://www.w3.org/XML/)
- Tutorial completo de XML: [https://www.w3schools.com/xml/](https://www.w3schools.com/xml/)
- Documentação do `xmerl` de Erlang para processamento de XML: [http://erlang.org/doc/apps/xmerl/](http://erlang.org/doc/apps/xmerl/)