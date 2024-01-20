---
title:                "Trabalhando com json"
html_title:           "C++: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Trabalhar com JSON é uma habilidade importante para programadores, especialmente no desenvolvimento web. JSON (JavaScript Object Notation) é um formato leve e legível de troca de dados, baseado em JavaScript, que é amplamente utilizado no transporte de informações entre um servidor e um cliente. Programadores trabalham com JSON para facilitar a comunicação entre diferentes sistemas e linguagens de programação, garantindo que os dados sejam transmitidos de forma eficiente e precisa.

## Como fazer:

Para trabalhar com JSON em C++, é necessário incluir a biblioteca padrão "json.hpp" no seu código. Em seguida, você pode criar um objeto JSON usando a sintaxe do C++ e adicionar pares de chave-valor usando o método "insert" e a função "make_pair". Veja um exemplo abaixo:

```C++
#include "json.hpp"

using json = nlohmann::json;

int main() {
  json obj = {
    {"nome", "Maria"},
    {"idade", 25},
    {"cidade", "São Paulo"}
  };

  obj.insert(std::make_pair("carro", "Fiat"));

  std::cout << obj << std::endl;
  return 0;
}
```

A saída desse código seria:

```
{
  "nome": "Maria",
  "idade": 25,
  "cidade": "São Paulo",
  "carro": "Fiat"
}
```

## Mergulho profundo:

JSON foi criado por Douglas Crockford em 2001, com o objetivo de fornecer um formato de dados simples e fácil de usar para troca de informações. O formato se tornou popular por ser leve e legível para humanos e por ser compatível com diferentes linguagens de programação. Embora seja amplamente utilizado, existem alternativas, como XML e YAML, que também podem ser usadas para a troca de dados. Além disso, a biblioteca "json.hpp" é baseada em templates do C++, o que torna seu código facilmente adaptável para diferentes projetos.

## Veja também:

- [C++ JSON Lib](https://github.com/nlohmann/json)
- [Histórico do JSON](https://www.json.org/json-en.html)