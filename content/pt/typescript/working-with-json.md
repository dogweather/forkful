---
title:                "TypeScript: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## Por que trabalhar com JSON em TypeScript

JSON (JavaScript Object Notations) é um formato de dados que tem se tornado cada vez mais popular na programação. Ele é usado para estruturar e armazenar informações de forma simples e legível. Em TypeScript, trabalhar com JSON pode facilitar muito o processo de manipulação e envio de dados em sua aplicação.

## Como fazer isso em TypeScript

Para trabalhar com JSON em TypeScript, é necessário usar a biblioteca `json()` do pacote `@types/node`, que inclui as definições de tipo para o formato JSON. Aqui está um exemplo de como realizar isso em seu código:

```typescript
import * as fs from "fs";
import { parse as jsonParse, stringify } from "json-bigint";

const jsonString = fs.readFileSync("arquivo.json", "utf-8"); //lê o arquivo.json

const jsonData = jsonParse(jsonString); //converte a string para um objeto JSON

jsonData["nome"] = "João"; //modifica o valor da propriedade "nome"

console.log(stringify(jsonData)); //converte o objeto JSON de volta para uma string e imprime no console
```

O código acima irá ler um arquivo JSON, converter os dados para um objeto JSON, fazer uma modificação e imprimir o resultado no console. Você pode também usar a função `stringify` para converter um objeto JavaScript em uma string JSON.

## Profundidade na manipulação de JSON

Ao trabalhar com JSON, é importante entender como ele é estruturado. Ele consiste em pares de "chave-valor", onde a chave é uma string que identifica o valor correspondente. A partir disso, é possível acessar e modificar propriedades específicas usando a notação de ponto ou colchetes.

Além disso, é possível usar a função `jsonParse()` para converter uma string JSON em um objeto JavaScript. Isso pode facilitar a manipulação dos dados e a utilização de métodos e propriedades do objeto.

## Veja também

- [Documentação do pacote @types/node (em inglês)](https://www.npmjs.com/package/@types/node)
- [Documentação do formato JSON (em português)](https://www.json.org/json-pt.html)
- [Outra forma de trabalhar com JSON em TypeScript (em inglês)](https://www.freecodecamp.org/news/working-with-json-in-typescript-3e0f80f4b427/)