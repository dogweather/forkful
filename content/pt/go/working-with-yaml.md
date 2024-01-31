---
title:                "Trabalhando com YAML"
date:                  2024-01-19
html_title:           "Arduino: Trabalhando com YAML"
simple_title:         "Trabalhando com YAML"

category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

YAML é um formato popular para configuração que combina legibilidade humana com forte suporte para estruturas de dados. Programadores o adotam por sua facilidade de uso e a capacidade de integrar-se bem com diversas linguagens de programação, incluindo Go.

## Como Fazer:

```Go
package main

import (
	"fmt"
	"log"

	"gopkg.in/yaml.v2"
)

// Config estrutura para os dados de configuração YAML
type Config struct {
	Database struct {
		User     string `yaml:"user"`
		Password string `yaml:"password"`
	} `yaml:"database"`
}

func main() {
	// YAML de exemplo
	yamlData := `
database:
  user: admin
  password: secreta
`
	var config Config

	// Deserializar YAML para struct
	err := yaml.Unmarshal([]byte(yamlData), &config)
	if err != nil {
		log.Fatalf("error: %v", err)
	}

	fmt.Printf("Usuário: %s\n", config.Database.User)
	fmt.Printf("Senha: %s\n", config.Database.Password)
}
```

Output:
```
Usuário: admin
Senha: secreta
```

## Mergulho Profundo

YAML começou em 2001, uma resposta às limitações do XML. Alternativas incluem JSON e TOML. Ao lidar com YAML em Go, usamos pacotes como `gopkg.in/yaml.v2` ou `gopkg.in/yaml.v3` para deserializar YAML em structs. A escolha da biblioteca influencia no suporte a funcionalidades YAML e na performance.

## Veja Também

- Documentação oficial YAML: https://yaml.org
- Biblioteca `yaml.v2` para Go: https://pkg.go.dev/gopkg.in/yaml.v2
- Biblioteca `yaml.v3` para Go, com mais funcionalidades: https://pkg.go.dev/gopkg.in/yaml.v3
- Tour de Go, para aprender mais sobre Go: https://tour.golang.org
