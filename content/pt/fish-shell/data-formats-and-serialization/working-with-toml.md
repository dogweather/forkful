---
date: 2024-01-26 04:21:36.353182-07:00
description: "Como fazer: Para ler e manipular TOML no Fish, voc\xEA pode usar uma\
  \ ferramenta como `yj`, que pode converter TOML para JSON. Veja como."
lastmod: '2024-03-13T22:44:47.029036-06:00'
model: gpt-4-0125-preview
summary: "Para ler e manipular TOML no Fish, voc\xEA pode usar uma ferramenta como\
  \ `yj`, que pode converter TOML para JSON."
title: Trabalhando com TOML
weight: 39
---

## Como fazer:
Para ler e manipular TOML no Fish, você pode usar uma ferramenta como `yj`, que pode converter TOML para JSON. Veja como:

```fish
# Instale o yj via Fisher
fisher install jorgebucaran/yj

# Converter TOML para JSON
echo 'title = "Exemplo de TOML"' | yj -tj

# Saída de exemplo
{"title":"Exemplo de TOML"}
```

Para escrever TOML, você inverte o processo:

```fish
# Converter JSON para TOML
echo '{"title":"Exemplo de JSON"}' | yj -jt

# Saída de exemplo
title = "Exemplo de JSON"
```

Para tarefas mais complexas, considere uma ferramenta CLI dedicada a TOML como `toml-cli`.

```fish
# Instalar toml-cli
pip install toml-cli

# Definir um valor no arquivo TOML
toml set pyproject.toml tool.poetry.version "1.1.4"

# Obter um valor do arquivo TOML
set version (toml get pyproject.toml tool.poetry.version)
echo $version
```

## Aprofundamento
TOML (Tom's Obvious, Minimal Language), introduzido por Tom Preston-Werner em 2013, é semelhante ao INI, mas com uma especificação definida e hierarquia de dados. JSON e YAML são as principais alternativas, mas eles têm seus compromissos: JSON não é tão amigável para humanos, enquanto YAML é mais complexo. O design do TOML prospera em cenários onde arquivos de configuração são frequentemente mantidos manualmente, equilibrando simplicidade e expressividade. Quando se trata de implementação, existem analisadores de TOML disponíveis para a maioria das linguagens de programação, incluindo TomlBombadil para Fish que pode se encaixar diretamente em seus scripts.

## Veja Também
- Especificação Oficial do TOML: https://toml.io
- `yj`, uma ferramenta para converter entre TOML, JSON, YAML e XML: https://github.com/jorgebucaran/yj
- `toml-cli`, uma utilidade de linha de comando para TOML: https://github.com/sdispater/toml-cli
