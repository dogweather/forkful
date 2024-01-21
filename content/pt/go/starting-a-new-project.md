---
title:                "Iniciando um novo projeto"
date:                  2024-01-20T18:03:42.585841-07:00
model:                 gpt-4-1106-preview
simple_title:         "Iniciando um novo projeto"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Começar um novo projeto em Go significa criar um espaço de trabalho onde você possa construir e organizar o seu código. Programadores fazem isso para estruturar suas ideias, testar novos conceitos ou simplesmente dar o pontapé inicial em algo incrível.

## Como Fazer:

Instale o Go (versão atual) no seu sistema e, em seguida, inicie um novo módulo:

```Go
// Abrir terminal e navegar para a pasta desejada
cd caminho/para/seu/novo/projeto

// Iniciar um novo módulo, substitua 'mymodule' pelo que desejar
go mod init mymodule
```

Crie um arquivo `main.go`:

```Go
package main

import "fmt"

func main() {
    fmt.Println("E ai, Go? Começando um novo projeto!")
}
```

Para rodar seu programa, use:

```Go
go run main.go
```

Sample output:

```
E ai, Go? Começando um novo projeto!
```

## Mergulho Profundo:

O Go foi desenvolvido pela Google em 2007 e apresentado ao público em 2009. Ele foi criado para ser eficiente, fácil de ler e escrever, possibilitando altas velocidades de compilação. Ao invés de usar o tradicional `makefile`, em Go utilizamos o `go mod init` para inicializar o gerenciamento de dependências e módulos, uma introdução que veio para simplificar a forma como gerenciamos bibliotecas e pacotes. Hoje em dia, existem outras ferramentas, como o `dep`, mas desde o Go 1.11, o módulo Go tem se tornado o padrão. Lembre-se, a estrutura do seu projeto pode variar dependendo da complexidade da aplicação; um único arquivo `main.go` pode ser suficiente para testar uma ideia, mas projetos maiores exigirão uma organização de diretórios e pacotes mais detalhada.

## Veja Também:

- Documentação oficial do Go: https://golang.org/doc/
- Tutorial interativo para aprender Go: https://tour.golang.org/
- Gestão de dependências e módulos em Go: https://blog.golang.org/using-go-modules 
- Uma discussão sobre organização de código em Go: https://golang.org/doc/code.html 
- Repo Go no GitHub para acompanhar a evolução da linguagem: https://github.com/golang/go