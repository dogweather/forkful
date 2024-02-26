---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:39.727628-07:00
description: "YAML, que significa YAML Ain't Markup Language (YAML N\xE3o \xE9 Uma\
  \ Linguagem de Marca\xE7\xE3o), \xE9 um formato de serializa\xE7\xE3o de dados leg\xED\
  vel por humanos.\u2026"
lastmod: '2024-02-25T18:49:44.517303-07:00'
model: gpt-4-0125-preview
summary: "YAML, que significa YAML Ain't Markup Language (YAML N\xE3o \xE9 Uma Linguagem\
  \ de Marca\xE7\xE3o), \xE9 um formato de serializa\xE7\xE3o de dados leg\xEDvel\
  \ por humanos.\u2026"
title: Trabalhando com YAML
---

{{< edit_this_page >}}

## O Que & Por Que?

YAML, que significa YAML Ain't Markup Language (YAML Não é Uma Linguagem de Marcação), é um formato de serialização de dados legível por humanos. Programadores o utilizam para arquivos de configuração, dumping de dados e armazenamento de dados hierárquicos devido à sua legibilidade e sintaxe fácil de entender em comparação com XML ou JSON.

## Como Fazer:

Para trabalhar com YAML em C++, uma escolha popular é a biblioteca `yaml-cpp`. Primeiro, certifique-se de ter o `yaml-cpp` instalado e devidamente vinculado ao seu projeto C++.

**Lendo um arquivo YAML:**

```cpp
#include <iostream>
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Node config = YAML::LoadFile("config.yaml");
    
    if(config["title"]) {
        std::cout << "Título: " << config["title"].as<std::string>() << std::endl;
    }
    
    return 0;
}
```

Dado um `config.yaml` que se pareça com isto:

```yaml
title: "Exemplo YAML"
```

Executando o código C++ acima produziria:

```
Título: Exemplo YAML
```

**Escrevendo em um arquivo YAML:**

```cpp
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Emitter out;
    out << YAML::BeginMap;
    out << YAML::Key << "title" << YAML::Value << "Exemplo YAML";
    out << YAML::EndMap;
    
    std::ofstream fout("output.yaml");
    fout << out.c_str();
    
    return 0;
}
```

Este código criará um `output.yaml` com o conteúdo:

```yaml
title: Exemplo YAML
```

Estes exemplos servem como uma introdução básica à leitura de e escrita em arquivos YAML em C++ usando a biblioteca `yaml-cpp`. Para estruturas mais complexas e casos de uso, explore a documentação do `yaml-cpp` para recursos como sequências, tags e técnicas mais avançadas de serialização e desserialização.
