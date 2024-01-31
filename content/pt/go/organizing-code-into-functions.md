---
title:                "Organizando o código em funções"
date:                  2024-01-26T01:11:07.373983-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizando o código em funções"

category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Organizar o código em funções trata de dividir seu código em partes reutilizáveis. Isso torna seu código mais limpo, fácil de ler e mais simples para depurar.

## Como fazer:
Aqui está um trecho em Go que mostra um bloco de código, seguido por uma versão refatorada usando funções:

```go
package main

import "fmt"

func main() {
    // Antes: Código Integrado
    fmt.Println("Calculando soma...")
    total := 0
    for i := 1; i <= 10; i++ {
        total += i
    }
    fmt.Println("Soma total é:", total)

    // Depois: Usando uma função
    fmt.Println("Calculando soma usando uma função...")
    soma := getSum(1, 10)
    fmt.Println("Soma total é:", soma)
}

// Função para calcular a soma dentro de um intervalo
func getSum(start, end int) int {
    total := 0
    for i := start; i <= end; i++ {
        total += i
    }
    return total
}
```

A saída de amostra para ambos os códigos, integrado e baseado em função, será a mesma:

```
Calculando soma...
Soma total é: 55
Calculando soma usando uma função...
Soma total é: 55
```

## Mergulho Profundo
Antes do conceito de funções emergir, a programação era largamente procedural, com código executado de cima para baixo. Conforme os programas cresciam, essa abordagem gerava ineficiência e repetição de código.

Linguagens introduziram funções como um mecanismo de abstração. Em Go, funções encapsulam blocos de código com uma tarefa específica, incentivando o princípio DRY (Don't Repeat Yourself - Não Se Repita). Elas aceitam parâmetros e podem retornar resultados.

Dicas úteis:
- Nomeie funções claramente; um bom nome explica o que uma função faz.
- Mantenha-as curtas; se uma função faz muito, divida-a.
- Funções podem retornar múltiplos valores, use isso para o tratamento de erros.
- Funções de ordem superior (funções que recebem ou retornam outras funções) são ferramentas poderosas em Go.

Alternativas às funções incluem código integrado (confuso para tarefas complexas) e métodos de objetos (parte do paradigma orientado a objetos disponível em Go através de structs).

## Veja Também
- [Go by Example: Functions](https://gobyexample.com/functions)
- [Effective Go: Function](https://golang.org/doc/effective_go#functions)
