---
title:                "Go: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que trabalhar com arquivos CSV em Go

Os arquivos CSV (Comma-Separated Values) são um formato comum para armazenar e compartilhar dados tabulares. Ao utilizar o Go para trabalhar com esses arquivos, é possível automatizar tarefas de processamento de dados e manipulação de informações. Além disso, a linguagem oferece recursos poderosos para realizar operações eficientes com arquivos CSV.

## Como trabalhar com arquivos CSV em Go

Para começar a trabalhar com arquivos CSV em Go, é necessário importar o pacote "encoding/csv". Em seguida, podemos utilizar a função "NewReader" para ler o arquivo CSV e obter um objeto "Reader" para percorrê-lo. Podemos então usar o método "Read" desse objeto para ler cada linha do arquivo e obter os dados em formato de slice. Veja um exemplo de código abaixo:

```Go
import (
    "encoding/csv"
    "fmt"
    "os"
)

func main() {
    // Criando um objeto Reader para ler o arquivo CSV
    file, err := os.Open("dados.csv")
    if err != nil {
        fmt.Println("Erro ao abrir o arquivo:", err)
        return
    }
    defer file.Close()

    reader := csv.NewReader(file)

    // Lendo cada linha do arquivo e exibindo na tela
    for {
        record, err := reader.Read()
        if err != nil {
            fmt.Println("Erro ao ler o arquivo:", err)
            break
        }
        fmt.Println(record)
    }
}
```

A saída do código acima será um slice contendo os dados de cada linha do arquivo CSV.

## Aprofundando-se em arquivos CSV com Go

Além da leitura de dados, o pacote "encoding/csv" do Go também oferece funções para escrever e manipular arquivos CSV. Por exemplo, podemos utilizar a função "NewWriter" para criar um objeto "Writer" e escrever dados em um novo arquivo CSV. Além disso, é possível utilizar ferramentas adicionais do Go, como o pacote "strconv", para converter valores de strings para outros tipos de dados, tornando a manipulação de dados CSV mais versátil.

## Veja também

- [Documentação do pacote "encoding/csv" em Go](https://golang.org/pkg/encoding/csv/)
- [Artigo sobre manipulação de arquivos CSV com Go](https://www.digitalocean.com/community/tutorials/how-to-manipulate-data-in-csv-files-using-go-pt)
- [Exemplo de uso do pacote "encoding/csv" em um projeto real](https://github.com/golang/example/tree/master/gotypes)