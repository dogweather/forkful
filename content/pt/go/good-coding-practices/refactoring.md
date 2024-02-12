---
title:                "Refatoração"
aliases:
- /pt/go/refactoring.md
date:                  2024-02-03T18:07:10.309882-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refatoração"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/refactoring.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Porquê?

Refatoração na programação envolve a reestruturação do código de computador existente — mudando a fatoração — sem alterar seu comportamento externo. Programadores empreendem esse processo para melhorar a legibilidade do código, reduzir a complexidade e aumentar a manutenibilidade, tornando o software mais fácil de entender e modificar.

## Como Fazer:

Em Go, a refatoração pode variar de ajustes simples no código a mudanças mais complexas. Vamos começar com um exemplo básico: simplificar uma função inicial em Go para melhor legibilidade e eficiência.

**Antes da Refatoração:**

```go
package main

import "fmt"

func CalculatePrice(quantity int, price float64) float64 {
    var total float64
    if quantity > 0 {
        total = float64(quantity) * price
    } else {
        total = 0
    }
    return total
}

func main() {
    fmt.Println(CalculatePrice(10, 5.99))  // Saída: 59.9
}
```

**Após Refatoração:**

```go
package main

import "fmt"

func CalculatePrice(quantity int, price float64) float64 {
    if quantity > 0 {
        return float64(quantity) * price
    }
    return 0
}

func main() {
    fmt.Println(CalculatePrice(10, 5.99))  // Saída: 59.9
}
```

Na versão refatorada, `else` é removido, o que simplifica o fluxo da função sem afetar seu resultado — um exemplo de uma técnica de refatoração básica, mas impactante, em Go.

Para um exemplo mais avançado, considere refatorar funções para usar interfaces, visando uma melhor reusabilidade e testabilidade:

**Antes da Refatoração:**

```go
package main

import "fmt"

type Logger struct{}

func (l Logger) Log(message string) {
    fmt.Println("Log:", message)
}

func ProcessData(data string, logger Logger) {
    // Imagine algum processamento de dados aqui
    logger.Log("Dados processados")
}

func main() {
    logger := Logger{}
    ProcessData("dados de exemplo", logger)
}
```

**Após Refatoração:**

```go
package main

import "fmt"

type Logger interface {
    Log(message string)
}

type ConsoleLogger struct{}

func (c ConsoleLogger) Log(message string) {
    fmt.Println("Log:", message)
}

func ProcessData(data string, logger Logger) {
    // O processamento de dados permanece inalterado
    logger.Log("Dados processados")
}

func main() {
    logger := ConsoleLogger{}
    ProcessData("dados de exemplo", logger)
}
```

Refatorar para usar uma interface (`Logger`) em vez de um tipo concreto (`ConsoleLogger`) melhora a flexibilidade da função e desacopla o processamento de dados da implementação específica de log.

## Aprofundamento

Refatorar em Go deve equilibrar simplicidade (uma das filosofias centrais do Go) com a flexibilidade necessária em grandes projetos de software. Dado a abordagem minimalista do Go para recursos — sem genéricos (até recentemente) e com forte ênfase na legibilidade — a linguagem naturalmente guia os desenvolvedores para estruturas de código mais simples e mais mantentáveis. No entanto, isso não significa que o código Go não se beneficie da refatoração; significa que a refatoração deve sempre priorizar clareza e simplicidade.

Historicamente, a falta de certos recursos no Go (por exemplo, genéricos antes do Go 1.18) levou a soluções criativas, mas às vezes complicadas, para reutilização de código e flexibilidade, tornando a refatoração para abstração uma prática comum. Com a introdução de genéricos no Go 1.18, os desenvolvedores de Go estão agora refatorando o código legado para aproveitar esse recurso para uma melhor segurança de tipos e reutilização de código, demonstrando a natureza evolutiva das práticas de refatoração em Go.

No entanto, o conjunto de ferramentas do Go, incluindo `gofmt` para formatação de código e `go vet` para identificar construções suspeitas, suporta a manutenção de bases de código limpas, reduzindo a necessidade de refatorações extensivas. Embora a refatoração seja uma ferramenta inestimável no arsenal de um programador Go, o uso sábio das funcionalidades da linguagem e das ferramentas desde o início pode ajudar a minimizar a necessidade de refatorações complexas mais tarde.
