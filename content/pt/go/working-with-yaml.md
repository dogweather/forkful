---
title:                "Trabalhando com YAML"
aliases:
- pt/go/working-with-yaml.md
date:                  2024-02-03T18:13:40.661751-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?

Trabalhar com YAML em Go envolve analisar arquivos YAML (YAML Ain't Markup Language), um padrão de serialização de dados amigável para humanos, em estruturas de dados Go e vice-versa. Programadores fazem isso para aproveitar a simplicidade e legibilidade do YAML para arquivos de configuração, configurações de aplicativos ou troca de dados entre serviços e componentes escritos em diferentes linguagens.

## Como:

Para trabalhar com YAML em Go, você precisará primeiro importar uma biblioteca que suporte análise e serialização de YAML, já que a biblioteca padrão do Go não inclui suporte direto para YAML. A biblioteca mais popular para esse propósito é "gopkg.in/yaml.v3". Veja como começar:

1. **Instalando o pacote YAML:**

```bash
go get gopkg.in/yaml.v3
```

2. **Analisando YAML em uma struct Go:**

Primeiro, defina uma struct em Go que corresponda à estrutura dos seus dados YAML.

```go
package main

import (
  "fmt"
  "gopkg.in/yaml.v3"
  "log"
)

type Config struct {
  Database struct {
    User     string `yaml:"user"`
    Password string `yaml:"password"`
  } `yaml:"database"`
}

func main() {
  var config Config
  data := `
database:
  user: admin
  password: secret
`
  err := yaml.Unmarshal([]byte(data), &config)
  if err != nil {
    log.Fatalf("error: %v", err)
  }
  fmt.Printf("Usuário: %s\nSenha: %s\n", config.Database.User, config.Database.Password)
}
```

**Saída de amostra:**

```
Usuário: admin
Senha: secret
```

3. **Serializando uma struct Go para YAML:**

Veja como converter uma struct Go de volta para YAML.

```go
package main

import (
  "fmt"
  "gopkg.in/yaml.v3"
  "log"
)

func main() {
  config := Config{
    Database: struct {
      User     string `yaml:"user"`
      Password string `yaml:"password"`
    }{
      User:     "admin",
      Password: "supersecret",
    },
  }

  data, err := yaml.Marshal(&config)
  if err != nil {
    log.Fatalf("error: %v", err)
  }
  fmt.Printf("---\n%s\n", string(data))
}
```

**Saída de amostra:**

```yaml
---
database:
  user: admin
  password: supersecret
```

## Aprofundamento:

O uso de YAML no desenvolvimento de software cresceu devido ao seu formato legível por humanos, tornando-o uma escolha ideal para arquivos de configuração, documentação ou formatos de troca de dados. Comparado ao JSON, seu contraparte, o YAML oferece comentários, tipos escalares e recursos de relacionamento, fornecendo um framework de serialização de dados mais rico. No entanto, sua flexibilidade e recursos vêm ao custo de complexidade na análise, levando a potenciais riscos de segurança quando não manuseado com cuidado (por exemplo, execução de código arbitrário).

A biblioteca "gopkg.in/yaml.v3" para Go é uma solução robusta para o processamento de YAML, encontrando um equilíbrio entre facilidade de uso e suporte abrangente de recursos. Até o presente momento, embora existam alternativas como "go-yaml/yaml" (a biblioteca por trás do "gopkg.in/yaml.v3"), a versão escolhida geralmente depende de requisitos específicos do projeto ou preferência pessoal. Ao lidar com conjuntos de dados massivos ou aplicações críticas para o desempenho, programadores podem considerar formatos mais simples como JSON para seu tempo de análise reduzido e sobrecarga de memória. No entanto, para arquivos de configuração ou configurações onde a legibilidade e facilidade de uso por humanos são primordiais, YAML continua sendo um forte concorrente no ecossistema Go.
