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

## Por que trabalhar com JSON?

JSON (JavaScript Object Notation) é um formato padrão amplamente utilizado para trocar dados entre diferentes sistemas e linguagens de programação. Ao trabalhar com JSON, os desenvolvedores podem facilmente armazenar, transmitir e recuperar dados de forma eficiente e confiável.

## Como fazer

Para começar a trabalhar com JSON em C++, você precisa primeiro incluir a biblioteca padrão `jsoncpp`. Em seguida, você pode criar um objeto JSON e preencher seus valores usando o operador `[]`. Aqui está um exemplo de código que cria um objeto JSON com dois valores, `nome` e `idade`, e imprime o resultado no console:

```C++
#include <iostream>
#include <jsoncpp/json/json.h>

int main() {
    Json::Value pessoa; // Criando um objeto JSON vazio
    pessoa["nome"] = "Maria";
    pessoa["idade"] = 25;
    std::cout << pessoa.toStyledString() << std::endl; // Imprime o objeto JSON formatado
    return 0;
}
```

A saída desse código será:

```
{
    "nome": "Maria",
    "idade": 25
}
```

Você também pode acessar os valores de um objeto JSON usando o operador `[]`. Por exemplo:

```C++
std::string nome = pessoa["nome"].asString(); // Acessando o valor "nome" como uma string
int idade = pessoa["idade"].asInt(); // Acessando o valor "idade" como um inteiro
```

Além disso, você pode criar arrays JSON usando o método `append` e acessar seus elementos usando índices numéricos.

## Mergulho Profundo

JSON é uma estrutura de dados bastante flexível e pode conter diferentes tipos de valores, incluindo strings, inteiros, booleanos, arrays e até mesmo outros objetos JSON. Para trabalhar com esses diferentes tipos de valores, a biblioteca `jsoncpp` oferece uma variedade de métodos e funções úteis, como `asString()`, `asInt()`, `asBool()`, `append()`, entre outros.

Além disso, você também pode serializar e desserializar objetos JSON usando `Json::StreamWriter` e `Json::StreamReader`, respectivamente. Isso pode ser útil ao ler e escrever JSON em arquivos de texto.

Cabe destacar que o formato JSON é bastante simples e intuitivo, mas é necessário prestar atenção na formatação correta dos objetos e valores para garantir que os dados sejam lidos e interpretados corretamente.

## Veja também

- [Documentação oficial do jsoncpp](https://github.com/open-source-parsers/jsoncpp/wiki)
- [Tutorial de JSON em C++](https://developer.mozilla.org/pt-BR/docs/Learn/JavaScript/Objects/JSON)
- [Outras bibliotecas JSON para C++](https://www.json.org/json-pt.html#c-json-libraries)