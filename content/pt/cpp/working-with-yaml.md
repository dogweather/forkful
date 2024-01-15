---
title:                "Trabalhando com yaml"
html_title:           "C++: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que

Você provavelmente já se deparou com arquivos YAML em seus projetos de programação. Mas você sabe por que eles são usados e como trabalhar com eles? Neste artigo, vamos explorar as razões por trás do uso de YAML e como você pode aproveitar ao máximo esse formato de dados.

## Como fazer

Antes de começarmos, é importante lembrar que YAML é uma linguagem de serialização de dados no formato de texto legível para humanos. Isso significa que ele pode ser facilmente lido e interpretado por humanos e máquinas. Para usar YAML em seus projetos C++, siga os passos abaixo:

```C++
#include <yaml-cpp/yaml.h>  // inclui o pacote YAML para C++
#include <iostream>         // inclui a biblioteca iostream para usar o console

int main() {
  // criando um objeto YAML
  YAML::Node myYAML;

  // adicionando campos
  myYAML["nome"] = "Maria";
  myYAML["idade"] = 25;
  myYAML["hobbies"] = { "leitura", "caminhada", "culinária" };

  // acessando os campos
  std::cout << "Nome: " << myYAML["nome"].as<std::string>() << std::endl;
  std::cout << "Idade: " << myYAML["idade"].as<int>() << std::endl;

  // acessando array de hobbies
  std::cout << "Hobbies: ";
  for (auto hobby : myYAML["hobbies"]) {
    std::cout << hobby.as<std::string>() << ", ";
  }
  std::cout << std::endl;
}
```

Saída:

```
Nome: Maria
Idade: 25
Hobbies: leitura, caminhada, culinária,
```

Com esse simples exemplo, você já pode começar a usar YAML em seus projetos C++. Você pode adicionar quantos campos quiser e acessá-los facilmente usando a sintaxe `myYAML["nome do campo"]`.

## Deep Dive

Agora que você já sabe como usar YAML em seus projetos C++, vamos nos aprofundar um pouco mais. YAML é um formato de dados extremamente flexível e versátil. Além de campos e arrays, ele também suporta valores booleanos, números, null e até mesmo tipos de dados personalizados.

Você também pode usar YAML em conjunto com outras bibliotecas, como a biblioteca Boost para processar arquivos YAML diretamente. E, se você precisar de mais recursos, pode dar uma olhada no pacote oficial yaml-cpp para C++.

Com YAML, você pode facilmente armazenar e recuperar dados de configuração, criar arquivos de testes, entre outras aplicações. Sua simplicidade e legibilidade tornam esse formato de dados uma ótima escolha ao trabalhar com projetos C++.

## Veja também

- Documentação oficial do YAML: https://yaml.org/
- Tutorial do yaml-cpp para C++: https://github.com/jbeder/yaml-cpp/wiki/Tutorial
- Biblioteca Boost para processar arquivos YAML em C++: https://www.boost.org/doc/libs/1_77_0/doc/html/property_tree.html#idp65097472