---
title:                "Go: Iniciando um novo projeto"
simple_title:         "Iniciando um novo projeto"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por que iniciar um novo projeto em Go?

Começar um novo projeto em Go pode ser uma ótima escolha para aqueles que desejam uma linguagem de programação rápida, moderna e eficiente. Com uma sintaxe limpa e concisa, além de uma grande comunidade de desenvolvedores, o Go oferece uma excelente base para construir projetos ambiciosos e inovadores.

## Como fazer:

Para começar a programar em Go, você precisará instalar o Go compiler e o Go tools. Em seguida, basta criar um arquivo com a extensão ".go" e começar a codificar! Aqui está um exemplo simples de um programa Hello World em Go:

```
package main

import "fmt"

func main() {
    fmt.Println("Olá, mundo!")
}
```

Ao executar esse programa, você deve ter a seguinte saída:

```
Olá, mundo!
```

O Go também possui recursos poderosos, como concurrency e channels, que permitem a criação de programas concorrentes de forma fácil e eficiente. Aqui está um exemplo de código que calcula a soma de dois números usando concurrency:

```
package main

import (
    "fmt"
    "math/rand"
)

func sum(a, b int, c chan int) {
    c <- a + b
}

func main() {
    rand.Seed(time.Now().UnixNano())

    c := make(chan int)
    a := rand.Intn(10)
    b := rand.Intn(10)
    go sum(a, b, c)
    fmt.Printf("A soma de %d e %d é %d\n", a, b, <-c)
}
```

Ao executar esse programa, você deve ter uma saída semelhante a esta:

```
A soma de 5 e 7 é 12
```

## Mergulho profundo:

Ao iniciar um novo projeto em Go, é importante seguir boas práticas de código e estruturação de projetos. Uma biblioteca popular para gerenciar dependências em Go é o "Go Modules", que permite que você especifique e gerencie as dependências de seus projetos de forma fácil e eficiente. Também é importante seguir as convenções de nomenclatura de pacotes e funções em Go, para manter seu código limpo e fácil de entender.

Além disso, existem muitos recursos e ferramentas disponíveis para ajudá-lo no desenvolvimento em Go, como a documentação oficial, a comunidade em fóruns e as inúmeras bibliotecas e frameworks disponíveis para diversas tarefas.

## Veja também:

- [Documentação oficial do Go](https://golang.org/doc/)
- [Fórum da comunidade Go](https://forum.golangbridge.org/)
- [Go Modules](https://github.com/golang/go/wiki/Modules)