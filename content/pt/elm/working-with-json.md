---
title:                "Trabalhando com JSON"
html_title:           "Arduino: Trabalhando com JSON"
simple_title:         "Trabalhando com JSON"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/working-with-json.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Trabalhar com JSON (JavaScript Object Notation) é manipular uma forma leve e padrão de troca de informações entre sistemas. Programadores o utilizam devido à sua simplicidade e capacidade de interagir com várias linguagens de programação, tornando a troca de dados entre cliente e servidor um processo suave.

## Como Fazer:

Elm torna o tratamento de JSON seguro por meio de decodificadores que transformam JSON em tipos Elm bem definidos. Veja um exemplo de um decodificador simples:

```Elm
import Json.Decode exposing (Decoder, string, int, field)

type alias Usuario = 
    { nome : String
    , idade : Int
    }

usuarioDecoder : Decoder Usuario
usuarioDecoder =
    Json.Decode.map2 Usuario
        (field "nome" string)
        (field "idade" int)

jsonExemplo : String
jsonExemplo =
    """
    { "nome": "Ana", "idade": 25 }
    """

main =
    Json.Decode.decodeString usuarioDecoder jsonExemplo
        |> String.fromInt
        |> text
```

Output:

```Elm
Ok { nome = "Ana", idade = 25 } -- resultado do decodificador
```

## Aprofundando

JSON surgiu dos subconjuntos da notação de objeto em JavaScript, mas atualmente é independente de linguagem, o que explica sua capilaridade. Em Elm, ao contrário de linguagens como JavaScript onde o JSON é manipulado diretamente, usamos decodificadores explícitos para conversão. Isto evita surpresas indesejadas em tempo de execução.

Alternativas para trabalhar com JSON em outras linguagens incluem bibliotecas como o Jackson em Java ou o JsonConvert no .NET. No entanto, a abordagem do Elm foca em garantir a segurança de tipos, o que a distingue de outras linguagens que são mais permissivas.

Detalhes de implementação no Elm incluem a necessidade de correspondência exata entre o JSON e o decodificador, assim como a habilidade de lidar com variações e estruturas de dados aninhadas mais complexas.

## Veja Também

- Documentação oficial do Elm para JSON Decode: [https://package.elm-lang.org/packages/elm/json/latest/](https://package.elm-lang.org/packages/elm/json/latest/)
- Tutorial completo do Elm: [https://guide.elm-lang.org/](https://guide.elm-lang.org/)
- JSON: [https://www.json.org/json-pt.html](https://www.json.org/json-pt.html)

Lembrete: esteja sempre ciente das versões e das mudanças na sintaxe entre atualizações das ferramentas que você usa.
