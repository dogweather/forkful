---
title:                "Go: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Go?

Criar arquivos temporários é uma prática comum em muitas linguagens de programação, incluindo Go. Esses arquivos são úteis para armazenar dados temporários durante a execução de um programa, como informações de cache ou de processamento. Além disso, eles podem ser uma solução para lidar com problemas de segurança relacionados ao armazenamento de dados sensíveis em disco. A seguir, mostraremos como criar um arquivo temporário em Go e como utilizá-lo em suas aplicações.

## Como fazer

A criação de um arquivo temporário em Go é bem simples e pode ser feita utilizando a função `ioutil.TempFile`. Veja um exemplo básico abaixo:

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"os"
)

func main() {
	// Criando um arquivo temporário no diretório atual
	file, err := ioutil.TempFile(".", "tempfile")
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	// Escrevendo dados no arquivo
	data := []byte("Exemplo de dados a serem armazenados no arquivo temporário")
	file.Write(data)

	// Fechando o arquivo
	file.Close()

	// Deletando o arquivo após o uso
	defer os.Remove(file.Name())

	// Exibindo o nome do arquivo temporário criado
	fmt.Println("Arquivo temporário criado:", file.Name())
}
```

Este código irá criar um arquivo temporário com o nome `tempfile` no diretório atual e escrever os dados especificados no mesmo. Ele também adiciona uma função `defer` para garantir que o arquivo seja excluído após a execução do programa.

O resultado deste código será:

```
Arquivo temporário criado: ./tempfile301762812
```

## Profundando mais

Além da função `ioutil.TempFile`, Go também possui uma biblioteca interna para manipulação de arquivos temporários chamada `os.TempDir`. Esta função retorna o caminho do diretório padrão para arquivos temporários do sistema operacional. Podemos utilizá-la para criar um arquivo temporário em um local específico. Veja um exemplo:

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"os"
)

func main() {
	// Obtendo o caminho do diretório de arquivos temporários
	tempDir := os.TempDir()

	// Criando um arquivo temporário no diretório especificado
	file, err := ioutil.TempFile(tempDir, "tempfile")
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	// Escrevendo dados no arquivo
	data := []byte("Exemplo de dados a serem armazenados no arquivo temporário")
	file.Write(data)

	// Fechando o arquivo
	file.Close()

	// Deletando o arquivo após o uso
	defer os.Remove(file.Name())

	// Exibindo o nome do arquivo temporário criado
	fmt.Println("Arquivo temporário criado:", file.Name())
}
```

No código acima, utilizamos a função `os.TempDir` para obter o caminho do diretório padrão e especificá-lo na função `ioutil.TempFile`. O resultado será o mesmo do exemplo anterior, mas o arquivo será criado no diretório de arquivos temporários do sistema operacional.

## Veja também

- [Documentação oficial da função `ioutil.TempFile` em Go](https://golang.org/pkg/io/ioutil/#TempFile)
- [Documentação oficial da função `os.TempDir` em Go](https://golang.org/pkg/os/#TempDir)
- [Artigo sobre segurança em arquivos temporários em Go](https://www.alexedwards.net/blog/sensitive-unprotected-temp-files)