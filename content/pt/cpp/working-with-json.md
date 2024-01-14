---
title:                "C++: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## Por que trabalhar com JSON

Nos dias de hoje, a troca de dados entre diferentes sistemas e linguagens de programação é cada vez mais comum. E é aí que entra o formato JSON (JavaScript Object Notation). Ele é amplamente utilizado para transmitir informações estruturadas de forma fácil de ler e interpretar. Trabalhando com JSON, você pode facilmente integrar seus projetos com outras plataformas e serviços.

## Como fazer

Para trabalhar com JSON em C++, podemos usar as bibliotecas de terceiros, tais como a **nlohmann/json** ou a **RapidJSON**. Neste artigo, vamos mostrar um exemplo de como fazer o parsing e o encoding de um objeto JSON usando a biblioteca nlohmann/json.

Primeiro, precisamos incluir a biblioteca em nosso código:

```C++
#include <nlohmann/json.hpp>
```

Em seguida, precisamos criar um objeto JSON e atribuir valores a ele:

```C++
nlohmann::json json_object = {
    {"name", "Maria"},
    {"age", 25},
    {"country", "Brazil"}
};
```

Agora, podemos facilmente acessar esses valores e exibi-los na tela:

```C++
std::cout << "Nome: " << json_object["name"] << std::endl;
std::cout << "Idade: " << json_object["age"] << std::endl;
std::cout << "País: " << json_object["country"] << std::endl;
```

A saída seria:

```
Nome: Maria
Idade: 25
País: Brazil
```

Podemos até mesmo manipular os dados e adicionar novos valores:

```C++
json_object["hobbies"] = {"reading", "traveling"};
json_object["age"] = 26;

std::cout << "Hobbies: " << json_object["hobbies"] << std::endl;
std::cout << "Nova idade: " << json_object["age"] << std::endl;
```

A saída seria:

```
Hobbies: ["reading", "traveling"]
Nova idade: 26
```

## Aprofundando-se no assunto

Uma das vantagens de trabalhar com JSON é que ele pode ser facilmente integrado com outros formatos de dados, como XML e CSV. Além disso, a maioria das linguagens de programação suporta o parsing e encoding de JSON, o que torna a troca de dados ainda mais fácil.

Ao trabalhar com grandes quantidades de dados JSON, é importante garantir que o seu código esteja otimizado e eficiente. Algumas bibliotecas, como a **RapidJSON**, oferecem recursos de parsing e encoding de alto desempenho.

Outro aspecto importante a ser considerado é a validação de dados JSON. É possível encontrar várias ferramentas online para validar o seu código JSON, o que pode ajudar a evitar erros e bugs em seu programa.

## Veja também

- [Guia de referência nlohmann/json](https://github.com/nlohmann/json/blob/develop/README.md)
- [Documentação do RapidJSON](https://rapidjson.org/)
- [Validador de JSON online](https://jsonlint.com/)