---
title:                "Tratamento de erros"
date:                  2024-01-26T00:52:51.253841-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tratamento de erros"

category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/handling-errors.md"
---

{{< edit_this_page >}}

## O Quê & Por Quê?

O tratamento de erros em Go é sobre capturar e responder a contratempos de execução de maneira elegante. Fazemos isso para evitar falhas e garantir que nossos programas atuem de maneira previsível, mesmo quando as coisas não vão bem.

## Como fazer:

Go utiliza um tratamento de erros explícito. Isso significa que você verificará se uma função retorna um erro toda vez que chamá-la. Sem Exceções. Veja como isso se parece:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	err := fazerAlgo()
	if err != nil {
		fmt.Println("Ops:", err)
		os.Exit(1)
	}
}

func fazerAlgo() error {
	// Fingindo que algo deu errado
	return fmt.Errorf("algo deu errado")
}
```

Execute isso, e você obterá:

```
Ops: algo deu errado
```

Mas e se tudo der certo?

```Go
func fazerAlgo() error {
	// Tudo certo desta vez
	return nil
}
```

Nenhuma saída. Legal, sem notícias são boas notícias.

## Mergulho Profundo:

Em Go, o tratamento de erros tem sido um ponto de controvérsia. Desde o início, Go decidiu contra exceções em favor de uma abordagem mais explícita, que alguns desenvolvedores adoram por sua simplicidade e outros acham verbosa. O tipo integrado `error` é uma interface. Qualquer tipo com um método `Error() string` a satisfaz. Isso está alinhado com o ethos de Go de simplicidade e explícito.

Alternativas? Há o duo `panic` e `recover`, mas eles são para casos excepcionais (com trocadilho intencional) quando o programa não pode continuar. Pense em `panic` como o botão de ejeção que você aperta quando sabe que não há volta. Use-o com parcimônia.

Quanto ao tratamento de erros convencional, Go 1.13 introduziu o encapsulamento de erros, facilitando a compreensão da "cadeia de erros" com funções como `errors.Is()` e `errors.As()`.

## Veja Também:

Para tudo sobre tratamento de erros em Go:

- O Blog Go sobre Tratamento de Erros: [https://blog.golang.org/error-handling-and-go](https://blog.golang.org/error-handling-and-go)
- Go Eficaz – Seção sobre tratamento de erros: [https://golang.org/doc/effective_go#errors](https://golang.org/doc/effective_go#errors)
- Documentação sobre Encapsulamento de Erros do Go 1.13: [https://golang.org/doc/go1.13#error_wrapping](https://golang.org/doc/go1.13#error_wrapping)
- Post de Dave Cheney sobre estratégias de tratamento de erros: [https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully](https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully)
