---
title:    "Go: Lendo um arquivo de texto."
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler e escrever arquivos de texto em Go?

Ler e escrever arquivos de texto é uma tarefa comum em muitos programas. No Go, essa tarefa é simplificada com a biblioteca padrão "os" que oferece funções para lidar com operações de arquivos. Neste post, vamos explorar como ler e escrever arquivos de texto em Go e como essa funcionalidade pode ser útil em seus programas.

## Como fazer

Para ler um arquivo de texto em Go, primeiro precisamos abrir o arquivo usando a função "os.Open". Isso nos dá acesso a um objeto do tipo "File" que possui diversos métodos para trabalhar com o arquivo. Em seguida, podemos utilizar o método "Read" para ler o conteúdo do arquivo em um slice de bytes. Veja o exemplo a seguir:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Abrir o arquivo para leitura
    file, err := os.Open("arquivo.txt")

    // Verificar se ocorreu algum erro ao abrir o arquivo
    if err != nil {
        fmt.Println("Erro ao abrir o arquivo")
        os.Exit(1)
    }

    defer file.Close()

    // Criar um slice de bytes para armazenar o conteúdo do arquivo
    data := make([]byte, 100)

    // Ler o conteúdo do arquivo e armazenar no slice
    count, err := file.Read(data)

    // Verificar se ocorreu algum erro ao ler o arquivo
    if err != nil {
        fmt.Println("Erro ao ler o arquivo")
        os.Exit(1)
    }

    fmt.Printf("Bytes lidos: %d\n", count)
    fmt.Printf("Conteúdo do arquivo: %s\n", data)
}
```

No exemplo acima, estamos lendo o conteúdo do arquivo "arquivo.txt" e armazenando em um slice de bytes de tamanho 100. Em seguida, imprimimos a quantidade de bytes que foram lidos e o conteúdo do arquivo. Vale ressaltar que o método "Read" lê o arquivo até encontrar o final do arquivo ou até atingir o tamanho do slice de bytes.

Para escrever em um arquivo de texto, podemos utilizar o método "Write" do objeto "File". Veja o exemplo a seguir:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Abrir o arquivo para escrita
    file, err := os.OpenFile("novo_arquivo.txt", os.O_CREATE|os.O_WRONLY, 0644)

    // Verificar se ocorreu algum erro ao abrir o arquivo
    if err != nil {
        fmt.Println("Erro ao abrir o arquivo")
        os.Exit(1)
    }

    defer file.Close()

    // Escrever "Hello, world!" no arquivo
    _, err = file.Write([]byte("Hello, world!"))

    // Verificar se ocorreu algum erro ao escrever no arquivo
    if err != nil {
        fmt.Println("Erro ao escrever no arquivo")
        os.Exit(1)
    }

    fmt.Println("Arquivo criado com sucesso")
}
```

Neste exemplo, estamos criando um novo arquivo de texto chamado "novo_arquivo.txt" e escrevendo a string "Hello, world!". Note que passamos alguns parâmetros extras para a função "os.OpenFile": "os.O_CREATE" cria o arquivo se ele não existir e "os.O_WRONLY" especifica que o arquivo será aberto para escrita.

## Passo a passo

1. Abra o arquivo usando a função "os.Open".
2. Utilize o método "Read" do objeto "File" para ler o conteúdo do arquivo em um slice de bytes.
3. Verifique se ocorreu algum erro ao ler o arquivo.
4. Utilize o método "Write" do objeto "File" para escrever no arquivo.
5. Verifique se ocorreu algum erro ao escrever no arquivo.

## Mais informações

Além dos métodos mencionados acima, o objeto "File" possui outros métodos úteis para ler e escrever arquivos de texto. Além disso, é possível utilizar outras bibliotecas, como "bufio", para auxiliar na leitura e escrita de arquivos. Vale a pena explorar a documentação oficial do Go para mais informações.

## Veja também

- [Documentação oficial do Go - Pacote "os"](https://golang.org/pkg/os/)
- [Exemplos de leitura e escrita de arquivos em Go](https://gobyexample.com/reading-files)