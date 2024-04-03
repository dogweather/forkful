---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:52.783392-07:00
description: "TOML (Tom's Obvious, Minimal Language) \xE9 um formato de arquivo de\
  \ configura\xE7\xE3o que \xE9 f\xE1cil de ler devido \xE0 sua sintaxe simples. Programadores\
  \ usam TOML\u2026"
lastmod: '2024-03-13T22:44:46.086136-06:00'
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language) \xE9 um formato de arquivo de configura\xE7\
  \xE3o que \xE9 f\xE1cil de ler devido \xE0 sua sintaxe simples."
title: Trabalhando com TOML
weight: 39
---

## O Que & Por Quê?

TOML (Tom's Obvious, Minimal Language) é um formato de arquivo de configuração que é fácil de ler devido à sua sintaxe simples. Programadores usam TOML para configurar configurações de aplicativos e dependências por causa de sua clareza e mapeamento direto para estruturas de dados, tornando-o uma escolha popular em muitos projetos Go para configurar e gerenciar configurações.

## Como Fazer:

Para começar a trabalhar com TOML em Go, você primeiro precisa incluir uma biblioteca que possa analisar arquivos TOML, já que a biblioteca padrão do Go não suporta TOML nativamente. O pacote `BurntSushi/toml` é uma escolha popular para isso. Primeiro, certifique-se de instalá-lo:

```bash
go get github.com/BurntSushi/toml
```

Aqui está um exemplo simples de como usá-lo. Considere que você tem um arquivo de configuração chamado `config.toml` com o seguinte conteúdo:

```toml
title = "Exemplo de TOML"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

Agora, você precisa criar uma estrutura Go que espelhe a estrutura TOML:

```go
package main

import (
    "fmt"
    "github.com/BurntSushi/toml"
)

type Config struct {
    Title    string
    Database Database `toml:"database"`
}

type Database struct {
    Server        string
    Ports         []int
    ConnectionMax int `toml:"connection_max"`
    Enabled       bool
}

func main() {
    var config Config
    if _, err := toml.DecodeFile("config.toml", &config); err != nil {
        fmt.Println(err)
        return
    }
    fmt.Printf("Título: %s\n", config.Title)
    fmt.Printf("Servidor da Base de Dados: %s\n", config.Database.Server)
}
```

Saída de exemplo:

```
Título: Exemplo de TOML
Servidor da Base de Dados: 192.168.1.1
```

## Aprofundando

TOML foi criado por Tom Preston-Werner, um dos cofundadores do GitHub, para oferecer um formato de arquivo de configuração direto que pode ser facilmente mapeado para uma tabela de hash e ser compreendido à primeira vista sem conhecimento prévio do formato. Ele contrasta com JSON ou YAML, que, embora também amplamente usados, podem ser menos amigáveis para arquivos de configuração por conta de chaves, aspas e problemas de indentação.

O pacote `BurntSushi/toml` em Go é uma biblioteca robusta que permite não apenas a decodificação, mas também a codificação de arquivos TOML, tornando-a uma escolha versátil para aplicações que precisam ler e escrever arquivos de configuração neste formato. No entanto, deve-se observar que com o avanço das tecnologias e a introdução de novas versões do Go, alternativas como `pelletier/go-toml` surgiram, oferecendo melhor desempenho e recursos adicionais como manipulação de árvores e suporte a consultas.

Embora o TOML seja uma ótima escolha para muitas aplicações, dependendo da complexidade da configuração da aplicação e das preferências pessoais ou da equipe, outros formatos como YAML ou JSON podem ser mais adequados, especialmente se a configuração requer estruturas de dados mais complexas que a natureza verbosa do TOML pode não capturar de forma elegante. No entanto, para configurações simples, legíveis e facilmente editáveis, o TOML, junto com o sistema de tipos forte do Go e as bibliotecas mencionadas, é uma excelente escolha.
