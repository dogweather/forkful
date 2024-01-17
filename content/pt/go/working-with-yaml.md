---
title:                "Trabalhando com yaml"
html_title:           "Go: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## O que é & Por que?

YAML é uma linguagem de marcação de dados usada em programação para estruturar e organizar dados em arquivos de texto. Programadores usam YAML porque é uma forma simples e legível de armazenar e compartilhar dados, especialmente em aplicações web e sistemas distribuídos.

## Como fazer:

Para trabalhar com YAML em Go, você precisará importar o pacote "gopkg.in/yaml.v2" em seu código. Aqui está um exemplo básico de como ler e imprimir dados de um arquivo YAML:

```
import (
    "fmt"
    "io/ioutil"
    "log"

    "gopkg.in/yaml.v2"
)

func main() {
    // lendo o arquivo YAML
    yamlFile, err := ioutil.ReadFile("dados.yml")
    if err != nil {
        log.Fatalf("Não foi possível ler o arquivo YAML: %v", err)
    }

    // criando uma variável para armazenar os dados
    var dados map[string]interface{}

    // mapeando os dados YAML na variável
    err = yaml.Unmarshal(yamlFile, &dados)
    if err != nil {
        log.Fatalf("Não foi possível fazer o parsing do arquivo: %v", err)
    }

    // imprimindo os dados
    fmt.Println(dados)
}
```

A saída desse código seria algo como:

```
map[person:map[age:30 name:John Doe] fruits:[banana apple orange]]
```

## Mergulho profundo:

O YAML foi criado em 2001 por Clark Evans e participantes da comunidade de linguagens Perl e Python. Ele foi criado para ser uma alternativa mais legível e amigável ao JSON. Enquanto o YAML pode ser usado para qualquer propósito, ele é mais comumente usado para configuração de aplicativos, gerenciamento de dados e intercâmbio de informações entre sistemas.

Alternativas ao YAML incluem formatos como JSON, XML e TOML. No entanto, o YAML continua sendo popular por sua sintaxe simples e legibilidade.

Ao trabalhar com YAML em Go, é importante entender a estrutura de dados que ele usa. YAML é baseado em linhas e espaçamento, o que pode causar problemas se a formatação for alterada. É recomendado usar uma ferramenta como "yaml.v2" para evitar erros de formatação e garantir uma estrutura de dados consistente.

## Veja também:

- Documentação oficial do pacote "yaml.v2": https://gopkg.in/yaml.v2
- Tutorial de introdução ao YAML em Go: https://golangbyexample.com/yaml-in-golang/
- Lista de alternativas ao YAML: https://alternativeto.net/software/yaml/