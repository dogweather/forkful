---
title:                "Trabalhando com TOML"
date:                  2024-01-26T04:22:34.143324-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/working-with-toml.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Trabalhar com TOML envolve analisar e codificar arquivos TOML (Tom's Obvious, Minimal Language) em Go. Os programadores optam pelo TOML pela sua legibilidade e fácil mapeamento para estruturas de dados, sendo uma escolha sólida para configurações.

## Como fazer:
Para trabalhar com TOML em Go, geralmente se usa uma biblioteca como `BurntSushi/toml`. Aqui está uma rápida olhada em como analisar um arquivo de configuração TOML:

```Go
package main

import (
    "fmt"
    "os"

    "github.com/BurntSushi/toml"
)

type Config struct {
    Title   string
    Owner   struct {
        Name string
    }
}

func main() {
    var config Config
    if _, err := toml.DecodeFile("config.toml", &config); err != nil {
        fmt.Println(err)
        return
    }
    fmt.Printf("Title: %s, Owner: %s\n", config.Title, config.Owner.Name)
}
```

Exemplo de `config.toml`:

```Toml
title = "Exemplo TOML"
[owner]
name = "Tom Preston-Werner"
```

Saída do exemplo:

```
Title: Exemplo TOML, Owner: Tom Preston-Werner
```

## Aprofundando
TOML, introduzido por Tom Preston-Werner em 2013, foi projetado para ser um formato de arquivo de configuração mínimo que é fácil de ler devido à sua clareza semântica. Desenvolvedores Go frequentemente usam TOML para configuração, preferindo-o a alternativas como JSON ou YAML pela sua direção clara e capacidade de representar hierarquias complexas com simplicidade.

Comparado ao YAML, que possui recursos complexos e potenciais preocupações de segurança, o design plano do TOML reduz a complexidade e os erros induzidos por digitação. E, ao contrário do JSON, o TOML suporta comentários, facilitando a explicação das configurações in-line.

Ao trabalhar com TOML em Go, há nuances a considerar. Tags de estrutura podem personalizar como suas estruturas mapeiam para estruturas TOML, e você também deve estar ciente de como os arrays TOML e tabelas inline são analisados em slices e mapas Go.

## Veja Também
- Especificação TOML: https://toml.io/en/
- Biblioteca BurntSushi/toml: https://github.com/BurntSushi/toml
- Uma comparação de formatos de arquivo de configuração: https://www.redhat.com/sysadmin/yaml-toml-json-differences
