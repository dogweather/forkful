---
title:                "Trabalhando com json"
html_title:           "Gleam: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## Por que trabalhar com JSON?

Se você é um desenvolvedor de software, provavelmente já se deparou com o formato JSON em seu trabalho. JSON (JavaScript Object Notation) é uma maneira simples e leve de armazenar e transmitir dados, tornando-o uma escolha popular para a comunicação entre aplicações web e servidores. Se você está trabalhando com dados estruturados ou está procurando uma maneira mais organizada de armazenar seus dados, aprender a trabalhar com JSON é uma habilidade valiosa a ter.

## Como fazer?

Agora que entendemos a importância de trabalhar com JSON, vamos ver como podemos usá-lo em nossos próprios projetos em Gleam. Primeiro, precisamos importar o módulo JSON em nosso código:

```Gleam
import gleam/json
```

Agora, podemos usar a função `from_string` para analisar uma string JSON e convertê-la em um objeto Gleam. Por exemplo, se tivermos a seguinte string JSON:

```json
{
  "nome": "Pedro",
  "idade": 27,
  "hobbies": ["viajar", "cozinhar", "jogar"]
}
```

Podemos convertê-la em um objeto Gleam usando o seguinte código:

```Gleam
let json_string = """
  {
    "nome": "Pedro",
    "idade": 27,
    "hobbies": ["viajar", "cozinhar", "jogar"]
  }
"""

let parsed_json = json.from_string(json_string)
```

Podemos acessar os valores do objeto Gleam usando a notação de ponto, por exemplo:

```Gleam
let nome = parsed_json.nome
let idade = parsed_json.idade
let primeiro_hobby = parsed_json.hobbies[0]
```

Também podemos usar a função `to_string` para converter um objeto Gleam em uma string JSON. Por exemplo, se tivermos um objeto Gleam com os mesmos valores que usamos acima, podemos convertê-lo em uma string JSON usando o seguinte código:

```Gleam
let gleam_obj = %{
  nome: "Pedro",
  idade: 27,
  hobbies: ["viajar", "cozinhar", "jogar"]
}

let json_string = json.to_string(gleam_obj)
```

Agora, `json_string` conterá a string JSON:

```json
{
  "nome": "Pedro",
  "idade": 27,
  "hobbies": ["viajar", "cozinhar", "jogar"]
}
```

## Mergulhe profundo

Embora esses sejam apenas alguns exemplos básicos de como trabalhar com JSON em Gleam, existem muitas outras funções e recursos disponíveis no módulo JSON. É importante entender a diferença entre `json.from_string` e `json.parse_string`, bem como a função `to_pretty_string` para formatar JSON de forma mais legível. Além disso, vale a pena explorar as opções disponíveis para trabalhar com JSON em projetos Gleam mais complexos.

## Veja também

- [Documentação do módulo JSON do Gleam](https://gleam.run/modules/json.html)
- [Tutorial do JSON no Gleam](https://pragmaticperl.com/issues/2017/11/pragmatic-parsley-16-gleam-goes-json/)
- [Exemplos de código Gleam com JSON](https://github.com/gleam-lang/gleam/blob/master/examples/json_reader.gleam)