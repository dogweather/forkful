---
title:                "Escrevendo para o erro padrão"
html_title:           "Go: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que

Você já se perguntou por que é importante escrever para o erro padrão (standard error) ao programar em Go? Bem, é simples: ao imprimir mensagens de erro no erro padrão, você pode facilmente identificar e depurar problemas em seu código. Isso facilita muito o processo de encontrar e corrigir erros em seu programa.

## Como fazer

Escrever para o erro padrão em Go é muito simples e pode ser feito usando a função `os.Stderr` e o pacote `fmt`. Veja um exemplo básico abaixo:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	// Escrevendo uma mensagem de erro para o erro padrão
	fmt.Fprintln(os.Stderr, "Algo deu errado!")

	// Saída:
	// Algo deu errado!
}
```
Como você pode ver, o pacote `fmt` possui uma função específica para escrever mensagens no erro padrão, a `Fprintln`. Basta fornecer o primeiro parâmetro como `os.Stderr` e sua mensagem como o segundo parâmetro.

Um ponto importante a ser destacado é que, ao contrário do `fmt.Println`, a função `fmt.Fprintln` não adiciona uma nova linha ao final da mensagem automaticamente. Por isso, é necessário incluir manualmente o caractere de quebra de linha `\n` para que a mensagem seja devidamente formatada ao ser impressa no erro padrão.

## Mergulho Profundo

Ao usar o `os.Stderr` para escrever mensagens no erro padrão, é importante lembrar que essa não é a única opção disponível. Em alguns casos, pode ser mais útil escrever para o erro de saída padrão (standard output) ou até mesmo para um arquivo externo.

Para escrever em outros arquivos, é possível usar o pacote `log` em conjunto com a função `log.New()` para criar um novo objeto de log. Veja um exemplo abaixo:

```Go
package main

import (
	"log"
	"os"
)

func main() {
	// Criando um novo objeto de log que escreve para um arquivo chamado "log.txt"
	file, err := os.Create("log.txt")
	if err != nil {
		log.Fatal(err)
	}
	logger := log.New(file, "", log.LstdFlags)

	// Escrevendo uma mensagem de erro no arquivo
	logger.Println("Erro encontrado!")

	// Saída no arquivo "log.txt":
	// 2021/04/12 10:27:40 Erro encontrado!
}
```

Além disso, o pacote `log` também possui funções úteis como `log.Panic()` e `log.Fatal()`, que podem ser usadas para imprimir uma mensagem de erro e encerrar a execução do programa com um status de erro.

## Veja também

Se você quiser saber mais sobre como trabalhar com o sistema de erros em Go, confira os links abaixo:

- [Pacote `fmt` no GoDoc](https://golang.org/pkg/fmt/)
- [Pacote `log` no GoDoc](https://golang.org/pkg/log/)
- [Tutorial sobre erros em Go (em inglês)](https://blog.golang.org/errors-are-values)