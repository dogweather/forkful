---
title:                "Go: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto em Go
Escrever arquivos de texto em Go é uma parte essencial do desenvolvimento de aplicativos. Esses arquivos são usados ​​para armazenar e transmitir informações entre diferentes partes do programa. Além disso, eles são muito úteis para a criação de logs e configurações do aplicativo.

## Como escrever um arquivo de texto em Go
Para começar a escrever um arquivo de texto em Go, primeiro precisamos criar um objeto de arquivo usando a função `Open()` do pacote `os`. Isso nos permitirá acessar e manipular o arquivo. Em seguida, podemos usar a função `WriteString()` para adicionar conteúdo ao arquivo. Finalmente, devemos fechar o arquivo usando a função `Close()` para garantir que todas as alterações sejam salvas.

```
package main

import (
	"fmt"
	"os"
)

func main() {
	// Criando um arquivo chamado "teste.txt" em modo de escrita
	file, err := os.Open("teste.txt")
	if err != nil {
		fmt.Println(err)
		return
	}
	// Escrevendo uma string no arquivo
	_, err = file.WriteString("Este é um exemplo de texto escrito em um arquivo usando Go!")
	if err != nil {
		fmt.Println(err)
		file.Close()
		return
	}
	// Fechando o arquivo
	err = file.Close()
	if err != nil {
		fmt.Println(err)
		return
	}
}
```

O resultado desse código será um novo arquivo de texto chamado "teste.txt" com o conteúdo adicionado.

## Mergulho profundo
Além da função `WriteString()`, há outras formas de escrever em arquivos de texto em Go. Podemos usar a função `Write()` para escrever bytes em vez de uma string. Também podemos usar a função `Fprintf()` para formatar e escrever dados em um arquivo. Além disso, é importante lembrar de manipular erros ao escrever em arquivos para garantir que todas as alterações sejam salvas corretamente.

## Veja também
- [Pacote Os em Go](https://golang.org/pkg/os/)
- [Manipulação de arquivos em Go](https://www.calhoun.io/working-with-files-in-go/)
- [Go tutorial: Trabalhando com arquivos](https://tutorialedge.net/golang/reading-and-writing-files-in-go/)