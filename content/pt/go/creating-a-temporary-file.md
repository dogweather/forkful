---
title:                "Go: Criando um arquivo temporário"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Go?

Ao trabalhar com programação, muitas vezes é necessário criar um arquivo temporário para armazenar dados temporários ou fazer cálculos intermediários. Em Go, a criação de arquivos temporários é uma tarefa simples e pode ser extremamente útil em várias situações.

## Como criar um arquivo temporário em Go?

A criação de um arquivo temporário em Go envolve algumas etapas simples:

1. Importe o pacote "os".
2. Use a função "TempDir" para obter um diretório temporário.
3. Use a função "Join" do pacote "path" para criar o caminho completo para o arquivo temporário.
4. Use a função "Create" do pacote "os" para criar o arquivo temporário.

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"path"
)

func main() {
	tempDir, err := ioutil.TempDir("", "tempfiles")
	if err != nil {
		panic(err)
	}
	defer os.RemoveAll(tempDir) //apaga o diretório temporário no final

	tempFile := path.Join(tempDir, "exemplo.txt")
	f, err := os.Create(tempFile)
	if err != nil {
		panic(err)
	}
	defer f.Close()

	fmt.Println("Arquivo temporário criado com sucesso:", f.Name())
}
```

O código acima irá criar um arquivo temporário no diretório especificado pelo sistema operacional. Se você executar o código várias vezes, verá que o nome do arquivo temporário é diferente a cada vez.

## Mergulhando mais a fundo

Além do exemplo acima, existem algumas opções adicionais que podem ser utilizadas ao criar um arquivo temporário:

- É possível especificar um prefixo para o nome do arquivo usando o segundo argumento da função "TempDir".
- A função "TempFile" do pacote "io/ioutil" pode ser usada para criar um arquivo temporário diretamente, sem a necessidade de usar a função "Join" e "Create".

Existem também outras formas de criar arquivos temporários em Go, como usando a função "ioutil.TempFile" ou o pacote "filepath/filepath".

## Veja também

- [Pacote "os" em Go](https://golang.org/pkg/os/)
- [Pacote "path" em Go](https://golang.org/pkg/path/)
- [Como criar, ler, gravar e apagar arquivos em Go](https://www.calhoun.io/creating-reading-and-writing-files-in-go/)