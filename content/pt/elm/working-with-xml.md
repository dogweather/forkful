---
title:                "Trabalhando com XML"
date:                  2024-01-26T04:30:38.291480-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com XML"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/working-with-xml.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Trabalhar com XML significa analisar, transformar e gerar documentos XML em Elm. Isso é feito para interagir com muitos serviços web e sistemas legados que usam XML como seu formato de dados.

## Como Fazer:
No Elm, você lida com XML usando o pacote `elm/xml`. Aqui está uma rápida olhada na análise de um trecho de XML:

```Elm
import Xml.Decode exposing (..)
import Xml.Decode.Pipeline exposing (..)

xmlString = """
<book id="123">
    <title>Elm in Action</title>
    <author>Robin Heggelund Hansen</author>
</book>
"""

type alias Book =
    { id : String
    , title : String
    , author : String
    }

bookDecoder : Decoder Book
bookDecoder =
    decode Book
        |> required "id" (attribute "id")
        |> required "title" (child "title" (content text))
        |> required "author" (child "author" (content text))

case Xml.Decode.fromString bookDecoder xmlString of
    Ok book ->
        -- Faça algo com o livro decodificado aqui
        Debug.toString book

    Err error ->
        -- Trate erros
        Debug.toString error
```

Saída de amostra, assumindo que não há erros:

```Elm
"{ id = \"123\", title = \"Elm in Action\", author = \"Robin Heggelund Hansen\" }"
```

## Mergulho Profundo
XML (eXtensible Markup Language) existe desde o final dos anos 90, uma época em que a web era cheia de texto e a necessidade de uma forma estruturada, mas flexível de transportar dados era crucial. Devido à verbosidade e complexidade, o XML perdeu algum espaço para o JSON. No entanto, o XML ainda é prevalente, especialmente em ambientes empresariais ou protocolos como SOAP.

A abordagem do Elm ao XML é funcional e segura no tipo. Usar o pacote `elm/xml` significa abraçar a filosofia Elm de explicitação e confiabilidade. Quando se trata de análise, o pacote fornece uma série de decodificadores que você compõe para lidar com a estrutura do XML.

Comparado a alternativas como o DOMParser do JavaScript ou o ElementTree do Python, o método do Elm pode parecer mais verboso, mas garante segurança. Sem exceções em tempo de execução por falta de campos ou incompatibilidade de tipos; se algo estiver errado, você recebe um erro em tempo de compilação.

As funções de decodificação `elm/xml` dependem do mapeamento de nós XML para tipos Elm. Você constrói decodificadores que espelham a forma dos seus dados, garantindo que o seu aplicativo Elm manipule o XML com tanto rigor quanto suas próprias estruturas de dados internas.

A geração de XML é menos comum em Elm, mas pode ser alcançada com o contraparte de `elm/xml` `Xml.Encode`.

## Veja Também
- Guia Elm sobre JSON que também se aplica à mentalidade XML: [https://guide.elm-lang.org/interop/json.html](https://guide.elm-lang.org/interop/json.html)
- Padrão XML pela W3C para um entendimento mais profundo do XML em si: [https://www.w3.org/XML/](https://www.w3.org/XML/)