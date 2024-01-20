---
title:                "Iniciando um novo projeto"
html_title:           "Javascript: Iniciando um novo projeto"
simple_title:         "Iniciando um novo projeto"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## O que & por qué?

Iniciar um novo projeto de programação é criar um esboço básico para o seu software futuro. Os programadores fazem isso para estruturar seu trabalho, definir metas e delinear a abordagem geral.

## Como fazer:

Criando um projeto simples "Olá, Mundo!" no Go:
```Go
package main

import "fmt"

func main() {
    fmt.Println("Olá, Mundo!")
}
```
A saída esperada:
```
Olá, Mundo!
```
Para compilar e executar:
```Go
go run hello.go
```

## Mergulho profundo

1. Contexto histórico: Go, também conhecido como Golang, foi projetado no Google por Robert Griesemer, Rob Pike e Ken Thompson, lançado em 2009. Ele nasceu da necessidade de lidar com a escala e a complexidade que outras linguagens de programação não conseguiam resolver.

2. Alternativas: Existem alternativas incontáveis para iniciar um novo projeto de programação. Você poderia, por exemplo, usar Python para um script mais rápido ou Java para desenvolvimento android.

3. Detalhes da implementação: Ao iniciar um novo projeto em Go, é importante organizar corretamente o seu espaço de trabalho Go e a estrutura do diretório do projeto. Go utiliza uma árvore de diretórios única global chamada 'GOPATH'.

## Veja também:

1. Documentação oficial da linguagem de programação Go: https://golang.org/doc/
2. Introdução à linguagem de programação Go no TutorialPoint: https://www.tutorialspoint.com/go/index.htm
3. Introdução ao Go (em Português) no Medium: https://medium.com/collabcode/introdu%C3%A7%C3%A3o-%C3%A0-linguagem-de-programa%C3%A7%C3%A3o-go-af0a2f2a5fa2