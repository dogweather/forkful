---
title:                "Criando um arquivo temporário"
html_title:           "Go: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Go?

Criar e utilizar arquivos temporários é uma prática comum em programação, especialmente quando se trabalha com dados sensíveis ou temporários. Em Go, isso é feito de forma simples e eficiente, permitindo que os desenvolvedores evitem o acúmulo de arquivos não utilizados ou expostos.

## Como fazer em Go?

A criação de um arquivo temporário em Go é feita utilizando a biblioteca "io/ioutil". Abaixo, temos um exemplo de como criar um arquivo temporário e escrever alguns dados nele:

```Go
import (
    "fmt"
    "io/ioutil"
)

func main() {
    // criando o arquivo temporário e capturando seu nome e eventuais erros
    arquivo, err := ioutil.TempFile("", "meuarquivo")
    if err != nil {
       fmt.Println(err)
    }

    // escrevendo dados no arquivo
    _, err = arquivo.Write([]byte("Meus dados"))
    if err != nil {
        fmt.Println(err)
    }

    // fechando e removendo o arquivo temporário após a utilização
    defer os.Remove(arquivo.Name())

    // exibindo o nome do arquivo
    fmt.Println(arquivo.Name())
}
```

A saída do código acima será algo como "/tmp/meuarquivo012345678". Além disso, o arquivo será automaticamente excluído após sua utilização, evitando assim a necessidade de limpezas manuais.

## Aprofundando

Ao criar um arquivo temporário em Go, é possível definir o diretório onde ele será criado e seu nome através dos dois argumentos passados para a função "TempFile". Caso o primeiro argumento seja uma string vazia ("") o arquivo será criado no diretório padrão definido pelo sistema operacional.

É importante ressaltar que é necessário utilizar o prefixo "os" para remover o arquivo temporário criado, como mostrado no exemplo acima. Além disso, é possível definir um sufixo para o nome do arquivo, que por padrão será um identificador único.

## Veja também

- [Documentação oficial da biblioteca ioutil](https://golang.org/pkg/io/ioutil/)
- [Guia de estilo do Go](https://github.com/golang/go/wiki/CodeReviewComments#identifier-names)