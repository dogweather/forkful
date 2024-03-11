---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:21.906718-07:00
description: "Tratar erros em Go envolve reconhecer e responder \xE0s condi\xE7\xF5\
  es de erro no seu programa. Os programadores se envolvem no tratamento de erros\
  \ para garantir\u2026"
lastmod: '2024-03-11T00:14:19.729886-06:00'
model: gpt-4-0125-preview
summary: "Tratar erros em Go envolve reconhecer e responder \xE0s condi\xE7\xF5es\
  \ de erro no seu programa. Os programadores se envolvem no tratamento de erros para\
  \ garantir\u2026"
title: Gerenciando erros
---

{{< edit_this_page >}}

## O Que & Por Que?

Tratar erros em Go envolve reconhecer e responder às condições de erro no seu programa. Os programadores se envolvem no tratamento de erros para garantir que suas aplicações possam se recuperar de forma elegante de situações inesperadas, levando a softwares mais robustos e confiáveis.

## Como fazer:

No Go, o tratamento de erros é gerenciado explicitamente usando o tipo `error`. Funções que podem falhar retornam um erro como seu último valor de retorno. Verificar se esse valor de erro é `nil` informará se ocorreu um erro.

```go
package main

import (
    "errors"
    "fmt"
)

func Compute(value int) (int, error) {
    if value > 100 {
        return 0, errors.New("o valor deve ser 100 ou menos")
    }
    return value * 2, nil
}

func main() {
    result, err := Compute(150)
    if err != nil {
        fmt.Println("Erro:", err)
    } else {
        fmt.Println("Resultado:", result)
    }
    
    // Lidando com um erro de forma elegante
    anotherResult, anotherErr := Compute(50)
    if anotherErr != nil {
        fmt.Println("Erro:", anotherErr)
    } else {
        fmt.Println("Resultado:", anotherResult)
    }
}
```

Saída de amostra para o código acima:
```
Erro: o valor deve ser 100 ou menos
Resultado: 100
```

Neste exemplo, a função `Compute` retorna um valor calculado ou um erro. O chamador trata o erro verificando se `err` não é `nil`.

## Aprofundamento

A abordagem de tratamento de erros do Go é deliberadamente direta e segura em termos de tipo, exigindo verificações explícitas de erros. Esse conceito contrasta com o tratamento de erros baseado em exceções visto em linguagens como Java e Python, onde os erros são propagados pela pilha de chamadas a menos que sejam capturados por um manipulador de exceções. A equipe do Go argumenta que o tratamento explícito de erros resulta em código mais claro e confiável, pois força os programadores a abordarem os erros imediatamente onde ocorrem.

No entanto, algumas críticas mencionam que esse padrão pode levar a um código verboso, especialmente em funções complexas com muitas operações propensas a erros. Em resposta, versões mais recentes do Go introduziram recursos de tratamento de erros mais sofisticados, como o encapsulamento de erros, tornando mais fácil fornecer contexto a um erro sem perder a informação do erro original. A comunidade também viu propostas para novos mecanismos de tratamento de erros, como check/handle, embora estes permaneçam em discussão até a minha última atualização.

A filosofia de tratamento de erros do Go enfatiza o entendimento e o planejamento para os erros como parte do fluxo normal do programa. Esta abordagem incentiva o desenvolvimento de software mais resiliente e previsível, ainda que com um possível aumento no código padrão. Padrões alternativos e bibliotecas existem para simplificar o tratamento de erros para casos particularmente complexos, mas o tipo `error` integrado do Go permanece a fundação do tratamento de erros no idioma.
