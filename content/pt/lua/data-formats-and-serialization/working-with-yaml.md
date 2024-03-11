---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:11.030275-07:00
description: "YAML, abrevia\xE7\xE3o de \"YAML Ain't Markup Language\" (YAML N\xE3\
  o \xE9 Uma Linguagem de Marca\xE7\xE3o), \xE9 um padr\xE3o de serializa\xE7\xE3\
  o de dados leg\xEDvel por humanos\u2026"
lastmod: '2024-03-11T00:14:20.448855-06:00'
model: gpt-4-0125-preview
summary: "YAML, abrevia\xE7\xE3o de \"YAML Ain't Markup Language\" (YAML N\xE3o \xE9\
  \ Uma Linguagem de Marca\xE7\xE3o), \xE9 um padr\xE3o de serializa\xE7\xE3o de dados\
  \ leg\xEDvel por humanos\u2026"
title: Trabalhando com YAML
---

{{< edit_this_page >}}

## O Que & Por Quê?

YAML, abreviação de "YAML Ain't Markup Language" (YAML Não é Uma Linguagem de Marcação), é um padrão de serialização de dados legível por humanos frequentemente usado para arquivos de configuração e troca de dados entre linguagens. Programadores utilizam YAML devido à sua simplicidade e legibilidade, tornando-o a escolha preferida para configurações, configurações de aplicativos diversos ou conteúdo que deve ser editável por não programadores.

## Como fazer:

Lua não possui suporte embutido para YAML, mas você pode trabalhar com arquivos YAML usando bibliotecas de terceiros, como `lyaml`. Esta biblioteca permite a codificação e decodificação de dados YAML com Lua. Primeiro, você precisará instalar o `lyaml` via LuaRocks, o gerenciador de pacotes do Lua:

```bash
luarocks install lyaml
```

### Decodificando YAML:

Suponha que você tenha o seguinte conteúdo YAML em um arquivo chamado `config.yaml`:

```yaml
database:
  host: localhost
  port: 3306
  username: usuário
  password: senha
```

Você pode decodificar este arquivo YAML em uma tabela Lua com o seguinte código:

```lua
local yaml = require('lyaml')
local file = io.open("config.yaml", "r")
local content = file:read("*all")
file:close()

local data = yaml.load(content)
for k,v in pairs(data.database) do
  print(k .. ": " .. v)
end
```

Quando você executar este script, ele deverá produzir a saída:

```output
host: localhost
port: 3306
username: usuário
password: senha
```

### Codificando YAML:

Para codificar tabelas Lua no formato YAML, você utiliza a função `dump` fornecida pela `lyaml`. Considerando que você queira criar uma representação YAML da seguinte tabela Lua:

```lua
local data = {
  website = {
    name = "Exemplo",
    owner = "Jane Doe",
    metadata = {
      creation_date = "2023-01-01",
      tags = {"blog", "pessoal", "lua"}
    }
  }
}

local yaml = require('lyaml')
local yaml_data = yaml.dump({data})
print(yaml_data)
```

O YAML resultante será:

```yaml
- website:
    metadata:
      creation_date: '2023-01-01'
      tags: [blog, pessoal, lua]
    name: Exemplo
    owner: Jane Doe
```

Seguindo esses padrões, programadores Lua podem gerenciar efetivamente dados YAML para uma variedade de aplicações. Essas operações com YAML são cruciais para o desenvolvimento de aplicações Lua versáteis que interagem suavemente com outras partes de um sistema ou diretamente com outros sistemas.
