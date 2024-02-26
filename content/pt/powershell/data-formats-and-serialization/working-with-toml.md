---
date: 2024-01-26 04:25:23.083745-07:00
description: "TOML, abrevia\xE7\xE3o de Tom's Obvious, Minimal Language (Linguagem\
  \ M\xEDnima e \xD3bvia do Tom), \xE9 um formato de serializa\xE7\xE3o de dados que\
  \ \xE9 f\xE1cil de ler devido \xE0\u2026"
lastmod: '2024-02-25T18:49:44.438994-07:00'
model: gpt-4-0125-preview
summary: "TOML, abrevia\xE7\xE3o de Tom's Obvious, Minimal Language (Linguagem M\xED\
  nima e \xD3bvia do Tom), \xE9 um formato de serializa\xE7\xE3o de dados que \xE9\
  \ f\xE1cil de ler devido \xE0\u2026"
title: Trabalhando com TOML
---

{{< edit_this_page >}}

## O Que & Por Quê?

TOML, abreviação de Tom's Obvious, Minimal Language (Linguagem Mínima e Óbvia do Tom), é um formato de serialização de dados que é fácil de ler devido à sua clareza semântica. Programadores o utilizam para arquivos de configuração, pois ele equilibra a legibilidade humana com a compatibilidade com máquinas.

## Como fazer:

No PowerShell, não existe um cmdlet nativo para interpretar TOML. Você normalmente usaria um módulo ou converteria TOML para JSON com uma ferramenta como `toml-to-json` se quiser trabalhar com o PowerShell. Veja como você faria isso com um módulo fictício `PowerShellTOML`:

```PowerShell
# Primeiro, instale o módulo (imaginário, para demonstração)
Install-Module PowerShellTOML

# Importe um arquivo TOML
$config = Import-TomlConfig -Path './config.toml'

# Acessando um valor
Write-Output $config.database.server

# Conteúdo TOML de exemplo em 'config.toml':
# [database]
# server = "192.168.1.1"
# ports = [ 8001, 8001, 8002 ]
# connection_max = 5000

# Saída de exemplo:
# 192.168.1.1
```

## Aprofundamento

TOML foi criado por Tom Preston-Werner, co-fundador do GitHub, como uma alternativa mais simples ao XML e YAML para arquivos de configuração. Sua primeira versão apareceu em 2013. TOML é comparável ao JSON, mas é projetado para ser mais amigável aos humanos, tornando-o uma boa escolha para configurações que são mantidas por pessoas. As alternativas incluem YAML, JSON e XML.

Em termos de implementação, um módulo PowerShell para TOML geralmente seria um invólucro ao redor de uma biblioteca TOML escrita em uma linguagem mais orientada a desempenho como C#. O PowerShell não tem suporte interno para TOML, por isso um módulo desse tipo é necessário para interagir convenientemente com o formato TOML.

## Veja Também

- Padrão TOML: https://toml.io/en/
- Repositório no GitHub para o módulo `toml` do PowerShell (se existir no momento da leitura): https://github.com/powershell/PowerShellTOML
- Uma introdução ao TOML: https://github.com/toml-lang/toml
- Comparação de formatos de serialização de dados: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
