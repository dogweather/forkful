---
title:    "Go: Criando um arquivo temporário"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Go?

Há diversas razões pelas quais alguém pode precisar criar um arquivo temporário em Go. Uma delas pode ser a necessidade de armazenar dados temporariamente durante a execução do programa, sem a necessidade de salvar esses dados permanentemente. Outra possível razão é a de precisar criar arquivos para serem usados como entrada ou saída em alguma função ou biblioteca específica.

## Como criar um arquivo temporário em Go

Criar um arquivo temporário em Go pode ser feito de forma fácil e eficiente utilizando a função `ioutil.TempFile()`. Esta função aceita dois argumentos: o primeiro é o diretório em que o arquivo temporário será criado e o segundo é um prefixo para o nome do arquivo. Aqui está um exemplo de como utilizar essa função:

```Go
package main

import (
	"fmt"
	"io/ioutil"
)

func main() {
	file, err := ioutil.TempFile("", "tempFile")
	if err != nil {
		fmt.Println("Não foi possível criar o arquivo temporário:", err)
		return
	}

	defer fmt.Println("Arquivo temporário criado:", file.Name())

	text := "Este é um arquivo temporário."

	if _, err = file.Write([]byte(text)); err != nil {
		fmt.Println("Não foi possível escrever no arquivo:", err)
	}

	if err = file.Close(); err != nil {
		fmt.Println("Não foi possível fechar o arquivo:", err)
	}
}

```

O código acima irá criar um arquivo temporário no diretório padrão do sistema com o prefixo "tempFile". Você também pode especificar o diretório desejado no primeiro argumento da função `ioutil.TempFile()`.

## Mergulho Profundo: Criando um arquivo temporário customizado

Além da função `ioutil.TempFile()`, também é possível criar um arquivo temporário customizado utilizando a biblioteca `os`. Aqui está um exemplo de como fazer isso:

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"os"
)

func main() {
	dir := os.TempDir()

	file, err := ioutil.TempFile(dir, "customTempFile")
	if err != nil {
		fmt.Println("Não foi possível criar o arquivo temporário:", err)
		return
	}

	fmt.Println("Arquivo temporário criado em:", file.Name())

	if err = file.Close(); err != nil {
		fmt.Println("Não foi possível fechar o arquivo:", err)
	}

	if err = os.Remove(file.Name()); err != nil {
		fmt.Println("Não foi possível remover o arquivo:", err)
	}
}
```

Neste exemplo, utilizamos a função `os.TempDir()` para obter o diretório padrão do sistema para arquivos temporários e criamos um arquivo com o prefixo "customTempFile". Em seguida, fechamos o arquivo e o removemos do diretório.

## Veja também

- [Documentação oficial do pacote `ioutil` em Go](https://golang.org/pkg/io/ioutil/)
- [Documentação oficial do pacote `os` em Go](https://golang.org/pkg/os/)
- [Tutorial de criação de arquivos temporários em Go](https://www.calhoun.io/creating-random-temp-files/)