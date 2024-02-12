---
title:                "Trabalhando com TOML"
aliases: - /pt/lua/working-with-toml.md
date:                  2024-01-26T04:24:56.829367-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/working-with-toml.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Trabalhar com TOML envolve analisar e gerar dados TOML (Tom's Obvious, Minimal Language) com Lua. Programadores utilizam TOML para arquivos de configuração devido à sua legibilidade e sintaxe simples que se traduz facilmente em uma estrutura de dados.

## Como fazer:
Primeiro, certifique-se de que o seu ambiente Lua possui um analisador de TOML. Usaremos `lua-toml` para este exemplo.

```Lua
local toml = require("toml")

-- Analisar string TOML
local toml_data = [[
title = "Exemplo TOML"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
]]

local data = toml.parse(toml_data)
print(data.title) -- "Exemplo TOML"

-- Gerar string TOML
local table_data = {
  title = "Exemplo TOML",
  owner = {
    name = "Tom Preston-Werner",
    dob = os.time({year=1979, month=5, day=27, hour=7, min=32})
  }
}

local toml_string = toml.encode(table_data)
print(toml_string)
```

Saída de Exemplo:
```
Exemplo TOML
```

## Mergulho Profundo
TOML foi criado por Tom Preston-Werner em 2013 como uma alternativa a outras linguagens de serialização de dados como XML e YAML, oferecendo um formato mais direto para representar dados de configuração. Embora o JSON seja onipresente, sua sintaxe pode ser pesada para arquivos de configuração. TOML brilha com uma sintaxe mais clara para humanos, lembrando arquivos .ini mas com capacidades de aninhamento e tipos de dados.

Alternativas ao TOML incluem JSON, YAML e XML. No entanto, TOML é especificamente projetado para configuração e é, argumentavelmente, mais simples que o YAML, mais legível que o JSON para fins de configuração, e menos prolixo que o XML.

Implementar o tratamento de TOML em Lua geralmente requer uma biblioteca de terceiros. Desempenho e recursos podem variar, desde análise básica até suporte completo de serialização. Ao lidar com grandes arquivos de configuração ou operações frequentes de leitura/escrita, considere o desempenho da biblioteca e a conformidade com a versão mais recente do TOML.

## Veja Também
- Especificação TOML: https://toml.io/en/
- Biblioteca `lua-toml`: https://github.com/jonstoler/lua-toml
- Comparação de Formatos de Serialização de Dados: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
