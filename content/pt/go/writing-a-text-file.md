---
title:                "Go: Escrevendo um arquivo de texto"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto?

Escrever um arquivo de texto é uma tarefa comum para muitos programadores, especialmente para aqueles que estão aprendendo a linguagem de programação Go. É uma habilidade fundamental para criar e armazenar informações úteis em um formato que possa ser facilmente lido e compreendido por outros programas.

## Como fazer

Escrever um arquivo de texto em Go é uma tarefa relativamente simples. Primeiro, precisamos importar o pacote `os` para que possamos lidar com operações no sistema operacional. Em seguida, podemos usar a função `Create` do pacote `os` para criar um novo arquivo de texto e o método `WriteString` para escrever nosso conteúdo nele. Veja um exemplo abaixo:

```Go
package main

import (
  "fmt"
  "os"
)

func main() {
  // Criando um novo arquivo de texto chamado "exemplo.txt"
  file, err := os.Create("exemplo.txt")
  if err != nil {
    fmt.Println(err)
  }
  
  // Escrevendo uma string no arquivo
  file.WriteString("Olá, mundo!")
  
  // Fechando o arquivo após uso
  defer file.Close()
}
```

Ao executar esse código, um arquivo de texto chamado "exemplo.txt" será criado no mesmo diretório que o arquivo Go. Ao abrirmos esse arquivo, poderemos ver o conteúdo "Olá, mundo!" escrito nele.

## Mergulhando mais fundo

Além do exemplo acima, existem outras maneiras de escrever um arquivo de texto em Go, como usar o pacote `io/ioutil` ou usar o método `Write` do pacote `os`. É importante também lembrar de sempre fechar o arquivo após o uso, para garantir que todas as alterações sejam salvas corretamente.

Além disso, ao escrever um arquivo de texto, também podemos adicionar formatações, como quebras de linha e alinhamento de texto, para torná-lo mais legível.

## Veja também

- [Documentação oficial do pacote `os` em Go](https://pkg.go.dev/os)
- [Tutorial de escrita de arquivos em Go](https://gobyexample.com/writing-files)
- [Exemplos de escrita de arquivos em Go](https://golangdocs.com/golang-write-file)