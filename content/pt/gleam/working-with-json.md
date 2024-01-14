---
title:                "Gleam: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## Por que usar JSON em Gleam?

JSON é uma forma popular de estruturar dados, e trabalhar com ele pode ser extremamente útil em projetos de programação. Com o Gleam, você pode facilmente manipular dados JSON em seus programas. Neste post, vamos explorar como trabalhar com JSON em Gleam.

## Como fazer:

Para começar, vamos importar o módulo "json" no início do nosso programa. Isso nos permitirá usar as funções da biblioteca para trabalhar com JSON.

```Gleam
import json
```

### Lendo dados JSON:

Para ler dados JSON em Gleam, podemos usar a função `decode` do módulo json. Essa função vai transformar uma string JSON em uma estrutura de dados que podemos manipular em nosso programa.

```Gleam
let json_string = "{\"nome\": \"Maria\", \"idade\": 30}"
let dados = json.decode(json_string)
```

Neste exemplo, estamos lendo uma string JSON com o nome e a idade de uma pessoa. A função `decode` irá criar uma struct chamada `dados` com os respectivos campos e valores.

### Escrevendo dados JSON:

Agora, se quisermos criar uma string JSON a partir de uma estrutura de dados no nosso programa, podemos usar a função `encode` do módulo json.

```Gleam
let dados = {nome: "João", idade: 25}
let json_string = json.encode(dados)
```

Neste caso, a função `encode` irá transformar a struct `dados` em uma string JSON, que pode ser usado para salvar ou enviar dados.

### Trabalhando com arrays:

Além de trabalhar com estruturas de dados JSON, também podemos manipular arrays em nosso programa Gleam. Para isso, basta usar a função `decode_array` para ler uma string JSON contendo um array.

```Gleam
let array_string = "[1, 2, 3, 4]"
let array = json.decode_array(array_string)
```

E para criar uma string JSON a partir de um array em nosso programa, podemos usar a função `encode_array`.

```Gleam
let array = [5, 6, 7, 8]
let array_string = json.encode_array(array)
```

## Profundando:

É importante lembrar que o módulo json do Gleam é imutável, o que significa que todas as funções retornam cópias dos dados originais ao invés de modificá-los diretamente. Portanto, é importante atribuir o resultado das funções a uma variável para poder usá-los posteriormente.

Outro detalhe importante é que, quando lidando com dados JSON, é necessário ter cuidado com os tipos de dados. Por exemplo, a função `decode` irá interpretar números como `int` ou `float`, dependendo do valor, mas também existe uma função `decode_number` que sempre retorna o tipo `Decimal` para números. Certifique-se de verificar a documentação para obter mais informações sobre os tipos de dados suportados.

## Veja também:

- Documentação oficial do Gleam sobre o módulo json: https://gleam.run/modules/json.html
- Exemplos de uso do módulo json em projetos Gleam: https://github.com/search?q=gleam+json&type=Code
- Tutoriais e dicas de Gleam para aprimorar suas habilidades na linguagem: http://www.fremingtonedge.com/gleam/