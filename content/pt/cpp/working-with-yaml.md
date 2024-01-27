---
title:                "Trabalhando com YAML"
date:                  2024-01-19
html_title:           "Arduino: Trabalhando com YAML"
simple_title:         "Trabalhando com YAML"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
YAML é um formato para dados legíveis por humanos, perfeito para configuração de aplicações. Programadores usam YAML para facilitar a manutenção e leitura de dados estruturados, além de promover facilidade de intercâmbio entre linguagens de programação.

## Como Fazer:
Para trabalhar com YAML em C++, você vai precisar de uma biblioteca de parsing, como a `yaml-cpp`. Instale primeiro, depois veja o exemplo de código abaixo:

```cpp
#include <yaml-cpp/yaml.h>
#include <iostream>
#include <fstream>

int main() {
    // Carregando o arquivo YAML
    YAML::Node config = YAML::LoadFile("config.yaml");
    
    // Supondo que 'config.yaml' tenha uma chave 'nome'
    std::string nome = config["nome"].as<std::string>();
    std::cout << "Nome: " << nome << std::endl;
    
    // Salvando novos dados em YAML
    YAML::Emitter out;
    out << YAML::BeginMap;
    out << YAML::Key << "idade";
    out << YAML::Value << 30;
    out << YAML::EndMap;
    
    std::ofstream fout("config.yaml");
    fout << out.c_str();
    return 0;
}
```

Saída esperada (no console):
```
Nome: João
```

Saída no arquivo `config.yaml` (atualizado):
```yaml
idade: 30
```

## Aprofundamento:
YAML surgiu em 2001 como uma alternativa ao XML, simples e mais fácil de ler. JSON e INI são outras opções, mas o YAML é único com sua ênfase na legibilidade. A implementação em C++ é feita através de bibliotecas externas, pois não há suporte nativo para YAML na STL (Standard Template Library).

## Veja Também:
- Documentação oficial do YAML: https://yaml.org
- Repositório Github da `yaml-cpp`: https://github.com/jbeder/yaml-cpp
- Tutorial detalhado sobre `yaml-cpp`: https://codedocs.xyz/jbeder/yaml-cpp/
- Comparação entre YAML, JSON e XML: https://www.json2yaml.com/what-is-yaml
