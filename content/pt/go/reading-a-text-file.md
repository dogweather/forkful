---
title:                "Lendo um arquivo de texto"
html_title:           "Bash: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Lidando com arquivos de texto no Go

Neste artigo, vamos aprender a ler arquivos de texto em Go, a linguagem de programação da Google.

## O Que & Por Que?

Ler um arquivo de texto é a atividade de extrair dados escritos desse arquivo. Programadores fazem isso para manipular ou analisar os dados contidos no arquivo.

## Como Fazer:

Aqui está um exemplo simples de como ler um arquivo de texto em Go:

```Go
package main

import (
	"fmt"
	"io/ioutil"
)

func main() {
	dados, erro := ioutil.ReadFile("arquivo.txt")
	if erro != nil {
		fmt.Println("Erro ao ler o arquivo:", erro)
		return
	}
	fmt.Println("Conteúdo do arquivo: ", string(dados))
}
```
Quando executado, este programa lê o arquivo "arquivo.txt" e imprime seu conteúdo.

## Mergulho Profundo:

Go fez sua estreia em 2007 e desde então tem se tornando uma escolha popular para lidar com operações de alto desempenho, como leitura e escrita de arquivos, graças a sua eficiência.

Alternativas para a leitura de arquivos incluem o uso de `os.Open` e `bufio.NewReader`, que permitem maior controle sobre o fluxo de dados, porém são mais complexos.

No Go, a leitura de um arquivo ocorre em três etapas: abertura do arquivo, leitura dos dados e, finalmente, fechamento do arquivo. É importante garantir que o arquivo seja fechado após a conclusão da leitura, mesmo que ocorra um erro durante o processo.

## Veja Também:

Para mais informações, consulte os seguintes links (em inglês):

1. [Pacote io/ioutil](https://golang.org/pkg/io/ioutil)
2. [Pacote os](https://golang.org/pkg/os)
3. [Pacote bufio](https://golang.org/pkg/bufio)
4. [Documentação oficial do Go](https://golang.org/doc)