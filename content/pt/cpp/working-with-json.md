---
title:                "Trabalhando com JSON"
html_title:           "Arduino: Trabalhando com JSON"
simple_title:         "Trabalhando com JSON"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## O Que & Por Que?

JSON, que significa JavaScript Object Notation, é um formato leve de troca de dados. Programadores o utilizam porque ele é fácil de ler e escrever para humanos e simples de interpretar e gerar para máquinas.

## Como Fazer:

Usarei a biblioteca `nlohmann/json` no exemplo abaixo. Primeiro, instale-a via gerenciador de pacotes ou inclua-a direto no projeto.

```C++
#include <iostream>
#include <nlohmann/json.hpp>

using json = nlohmann::json;

int main() {
    // Criando um objeto JSON
    json j;
    j["nome"] = "João";
    j["idade"] = 25;
    j["interesses"] = {"futebol", "programação"};

    // Convertendo para string e imprimindo
    std::string jsonString = j.dump();
    std::cout << jsonString << std::endl;

    // Lendo JSON de uma string
    std::string user_info = R"({"nome":"Ana","idade":30,"interesses":["música","viagens"]})";
    json j2 = json::parse(user_info);
    
    std::cout << "Nome: " << j2["nome"] << ", Idade: " << j2["idade"] << std::endl;
    
    return 0;
}
```
Quando rodar o programa, você verá:

```
{"idade":25,"interesses":["futebol","programação"],"nome":"João"}
Nome: Ana, Idade: 30
```

## Mergulho Profundo

JSON surgiu em 2001, inserido como um subconjunto de JavaScript, mas hoje é independente e adotado por muitas linguagens, incluindo C++. Outras opções para troca de dados incluem XML e YAML. Na prática, JSON tende a ser mais direto e menos verboso que o XML. Implementar o suporte a JSON em C++ geralmente requer uma biblioteca externa desde que o padrão da linguagem não inclua JSON nativamente.

## Veja Também:

- Documentação oficial do JSON: https://www.json.org/json-pt.html
- GitHub da `nlohmann/json`: https://github.com/nlohmann/json
- Comparação entre JSON e XML: https://www.w3schools.com/js/js_json_xml.asp