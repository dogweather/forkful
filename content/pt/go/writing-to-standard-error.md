---
title:                "Go: Escrevendo para a saída de erro padrão"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Por que escrever para o erro padrão em Go?

Escrever para o erro padrão, também conhecido como standard error ou `stderr`, é uma prática comum em programação, especialmente em linguagens de programação de sistema como Go. Esse recurso permite que os desenvolvedores tenham mais controle sobre os erros e mensagens que seus programas produzem, facilitando a depuração e o entendimento do código. Neste artigo, vamos dar uma olhada em como escrever para o erro padrão em Go e por que isso é importante.

## Como fazer

Para escrever para o erro padrão em Go, podemos usar a função `fmt.Fprintln()` ou `fmt.Fprintf()` do pacote `fmt`, que permitem imprimir uma mensagem diretamente para o `stderr`. Por exemplo:

```Go
package main

import "fmt"

func main() {
	fmt.Fprintln(os.Stderr, "Esta é uma mensagem de erro")
}
```

Neste exemplo, passamos a mensagem desejada como segundo argumento da função `fmt.Fprintln()`, que será automaticamente escrita no `stderr`. Você também pode formatar a mensagem usando a função `fmt.Sprintf()` antes de passá-la para `fmt.Fprintf()`.

## Mergulho profundo

Agora que sabemos como escrever para o erro padrão em Go, é importante entender quando e por que devemos fazer isso. O erro padrão é usado para mensagens de erro, alertas e outras informações importantes que devem ser exibidas ao usuário. Além disso, muitas ferramentas de linha de comando, como o `grep` e o `make`, usam o erro padrão para exibir informações de estado e feedback ao usuário. Por padrão, o `stdout` (standard output) é usado para a saída normal do programa, enquanto o `stderr` é usado para informações importantes.

Além disso, usar o `stderr` também pode ser útil para debugar seu código. Como o `stderr` geralmente é usado para mensagens de erro, é mais fácil localizar e identificar problemas em seu código quando essas mensagens são escritas diretamente no `stderr`.

## Veja também

Para mais informações sobre como escrever para o erro padrão em Go, consulte os links a seguir:

- [Pacote fmt na documentação oficial do Go](https://golang.org/pkg/fmt/)
- [Artigo sobre o uso de stderr em Go](https://blog.golang.org/error-handling-and-go)
- [Exemplo de código usando o erro padrão em Go](https://gist.github.com/bgentry/5c0f2277f47f67e59fc9)