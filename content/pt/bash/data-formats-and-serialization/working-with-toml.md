---
aliases:
- /pt/bash/working-with-toml/
date: 2024-01-26 04:18:58.542423-07:00
description: "TOML, abrevia\xE7\xE3o de Tom's Obvious, Minimal Language, \xE9 um formato\
  \ de serializa\xE7\xE3o de dados. Os programadores curtem pela sua simplicidade\
  \ e legibilidade;\u2026"
lastmod: 2024-02-18 23:08:58.348480
model: gpt-4-0125-preview
summary: "TOML, abrevia\xE7\xE3o de Tom's Obvious, Minimal Language, \xE9 um formato\
  \ de serializa\xE7\xE3o de dados. Os programadores curtem pela sua simplicidade\
  \ e legibilidade;\u2026"
title: Trabalhando com TOML
---

{{< edit_this_page >}}

## O Quê & Por Quê?
TOML, abreviação de Tom's Obvious, Minimal Language, é um formato de serialização de dados. Os programadores curtem pela sua simplicidade e legibilidade; é ótimo para arquivos de configuração, com uma vibe similar ao YAML, mas menos pesado que o JSON para um humano.

## Como Fazer:
Primeiro, instale o `toml-cli` para brincar com TOML no Bash. Útil para ler ou editar arquivos TOML rapidamente.

```Bash
# Instalar toml-cli, nosso pequeno ajudante para tarefas TOML
pip install toml-cli

# Imagine que você tem um arquivo TOML, 'config.toml'
echo -e 'title = "TOML Demo"\n\n[owner]\nname = "Tom"\ndob = 1979-05-27T07:32:00Z' > config.toml

# Ler um valor
toml get config.toml owner.name
# Saída: Tom

# Definir um valor
toml set config.toml 'owner.dob' '2000-01-01T00:00:00Z'
# Dica profissional: Use aspas para chaves com pontos ou caracteres estranhos!
```

## Aprofundamento
Nascido do desgosto pelos obstáculos do JSON para humanos, TOML surgiu por volta de 2013. Tom Preston-Werner, co-fundador do GitHub, queria algo super legível. YAML e INI eram alternativas, mas o TOML é como o melhor de ambos.

Pá, você tem dados aninhados e arrays, sem as armadilhas do YAML e as chaves curvas do JSON. TOML agora é a escolha para configuração no Cargo do Rust, o que fala do seu crescimento no mundo do desenvolvimento. É impulsionado por uma especificação, mantendo as coisas apertadas e bem definidas. Você pegará parsers em quase qualquer linguagem, tornando-o amplamente adotável.

## Veja Também
- Repositório Oficial do TOML no GitHub: https://github.com/toml-lang/toml
- toml-cli no PyPI: https://pypi.org/project/toml-cli/
- Comparação de formatos de serialização de dados: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
