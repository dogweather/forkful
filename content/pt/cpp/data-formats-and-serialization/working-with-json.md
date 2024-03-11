---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:59.698556-07:00
description: "JSON (JavaScript Object Notation) \xE9 um formato leve para armazenamento\
  \ e transporte de dados, tornando-se um excelente meio para troca de dados entre\u2026"
lastmod: '2024-03-11T00:14:20.631172-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) \xE9 um formato leve para armazenamento\
  \ e transporte de dados, tornando-se um excelente meio para troca de dados entre\u2026"
title: Trabalhando com JSON
---

{{< edit_this_page >}}

## O Que & Por Quê?

JSON (JavaScript Object Notation) é um formato leve para armazenamento e transporte de dados, tornando-se um excelente meio para troca de dados entre servidores e aplicações web. Programadores usam JSON devido à sua fácil leitura por humanos e simples análise por máquinas, especialmente quando trabalham em aplicações que requerem troca de dados pela internet ou configurações de ajustes.

## Como Fazer:

Em C++, não há suporte nativo para JSON, mas bibliotecas de terceiros como nlohmann/json tornam isso direto. Aqui está como usá-la para tarefas básicas:

Primeiro, certifique-se de que você tem a biblioteca instalada. Se você estiver usando um gerenciador de pacotes como vcpkg ou Conan, pode facilmente adicionar `nlohmann/json` ao seu projeto.

### Analisando JSON de uma string

```cpp
#include <iostream>
#include <nlohmann/json.hpp>

int main() {
    // Dados JSON como uma string
    std::string jsonData = "{\"name\":\"John\", \"age\":30, \"city\":\"New York\"}";

    // Analisar string JSON
    auto jsonObject = nlohmann::json::parse(jsonData);

    // Acessando dados
    std::cout << "Nome: " << jsonObject["name"] << "\n"
              << "Idade: " << jsonObject["age"] << "\n"
              << "Cidade: " << jsonObject["city"] << std::endl;

    return 0;
}
```

**Saída de exemplo:**

```
Nome: John
Idade: 30
Cidade: New York
```

### Gerando JSON

Criar dados JSON é tão simples quanto; você simplesmente atribui valores a um objeto `nlohmann::json`.

```cpp
#include <nlohmann/json.hpp>
#include <iostream>

int main() {
    // Criando um objeto JSON
    nlohmann::json jsonObject;
    jsonObject["name"] = "Jane";
    jsonObject["age"] = 25;
    jsonObject["city"] = "Los Angeles";

    // Converter objeto JSON para string e imprimir
    std::string jsonString = jsonObject.dump(4); // Argumento 4 para impressão bonita
    std::cout << jsonString << std::endl;

    return 0;
}
```

**Saída de exemplo:**

```
{
    "name": "Jane",
    "age": 25,
    "city": "Los Angeles"
}
```

Esses exemplos demonstram a funcionalidade núcleo para trabalhar com JSON em C++ usando a biblioteca `nlohmann/json`. Com esses conceitos básicos, você pode analisar e gerar JSON para várias aplicações, desde arquivos de configuração até a troca de dados em aplicações em rede.
