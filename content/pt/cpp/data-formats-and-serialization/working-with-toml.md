---
date: 2024-01-26 04:19:48.872265-07:00
description: "TOML (Tom's Obvious, Minimal Language - Linguagem M\xEDnima e \xD3bvia\
  \ do Tom) \xE9 um formato de serializa\xE7\xE3o de dados f\xE1cil de ler devido\
  \ \xE0 sua sem\xE2ntica clara.\u2026"
lastmod: '2024-03-13T22:44:46.902785-06:00'
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language - Linguagem M\xEDnima e \xD3bvia do\
  \ Tom) \xE9 um formato de serializa\xE7\xE3o de dados f\xE1cil de ler devido \xE0\
  \ sua sem\xE2ntica clara.\u2026"
title: Trabalhando com TOML
weight: 39
---

## O Que & Por Que?
TOML (Tom's Obvious, Minimal Language - Linguagem Mínima e Óbvia do Tom) é um formato de serialização de dados fácil de ler devido à sua semântica clara. Programadores usam TOML para arquivos de configuração porque ele equilibra a legibilidade humana com a análise por máquinas.

## Como Fazer:
Para trabalhar com TOML em C++, você precisará de uma biblioteca como `toml++`. Aqui está um guia rápido:

```C++
#include <toml++/toml.h>
#include <iostream>
#include <fstream>

int main() {
    // Analisar TOML de um arquivo
    std::ifstream ifs("config.toml");
    auto config = toml::parse(ifs);

    // Acessar um valor
    std::string title = config["title"].value_or("Sem Título");
    std::cout << "Título: " << title << '\n';

    // Modificar e salvar TOML
    config["title"] = "Novo Título";
    std::ofstream ofs("config.toml");
    ofs << config;
}
```

Exemplo de `config.toml`:
```toml
title = "Exemplo"
```

Saída de exemplo:
```plaintext
Título: Exemplo
```

## Aprofundamento
TOML foi criado por Tom Preston-Werner em 2013 como uma alternativa ao YAML e JSON. Ele é projetado para ser simples e explícito, principalmente para arquivos de configuração. Diferente do JSON, o TOML foca em ser inequívoco, o que significa que é determinístico em como o documento é analisado.

Alternativas ao TOML incluem o YAML, que é mais permissivo no que é permitido, embora às vezes ao custo da previsibilidade. O JSON, outra alternativa, é bastante estrito em estrutura, mas não tão amigável para configurações humanas devido à falta de comentários e sua sintaxe pesada de chaves.

Na implementação, `toml++` é uma biblioteca apenas de cabeçalhos C++17 que está em conformidade com a última especificação TOML. Ela fornece uma interface semelhante a DOM para navegar e manipular dados TOML, tornando-a direta para integrar em projetos. A biblioteca cuida da análise, validação e geração de saída, permitindo-lhe obter e definir dados TOML usando tipos C++.

## Veja Também
- O repositório GitHub do TOML: https://github.com/toml-lang/toml
- `toml++`, uma biblioteca C++ para TOML: https://github.com/marzer/tomlplusplus
- A documentação oficial do TOML com explicações detalhadas do formato: https://toml.io/en/
