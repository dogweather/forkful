---
title:                "Trabalhando com csv"
html_title:           "Go: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que usar CSV em Go?

Se você trabalha com dados estruturados em sua aplicação Go, é provável que já tenha se deparado com o formato CSV (Comma Separated Values). Este formato é amplamente utilizado para armazenar e compartilhar dados tabulares, e é uma ótima opção para armazenar grandes conjuntos de dados.

## Como trabalhar com CSV em Go

Para trabalhar com arquivos CSV em Go, primeiro precisamos importar o pacote "encoding/csv" em nosso código. Em seguida, podemos utilizar a função "csv.NewReader" para criar um leitor que irá ler nosso arquivo CSV.

```
import (
    "encoding/csv"
    "os"
)

func main() {
    // Criando um leitor para o arquivo CSV
    file, _ := os.Open("arquivo.csv")
    reader := csv.NewReader(file)

    // Lendo os dados do arquivo linha por linha
    for {
        record, err := reader.Read()
        if err != nil {
            break
        }
        // Imprimindo a linha lida do arquivo
        fmt.Println(record)
    }
}
```

O código acima irá imprimir cada linha do arquivo CSV, separando os valores por vírgula. Além disso, podemos utilizar a função "csv.NewWriter" para criar um escritor que nos permitirá escrever dados em um arquivo CSV. Veja um exemplo abaixo:

```
import (
    "encoding/csv"
    "os"
)

func main() {
    // Criando um escritor para o arquivo CSV
    file, _ := os.Create("novo_arquivo.csv")
    writer := csv.NewWriter(file)

    // Criando uma linha com dados a serem escritos no arquivo
    linha := []string{"nome", "sobrenome", "idade", "email"}
    // Escrevendo a linha no arquivo
    writer.Write(linha)

    // Finalizando o escritor e salvando as mudanças no arquivo
    writer.Flush()
}
```

## Aprofundando no uso de CSV em Go

Além de ler e escrever em arquivos CSV, o pacote "encoding/csv" possui outras funcionalidades interessantes, como definir delimitadores personalizados, trabalhar com cabeçalhos de coluna e lidar com possíveis erros durante o processamento do arquivo.

É importante lembrar que, como o formato CSV é simples e bastante flexível, é responsabilidade do desenvolvedor garantir que os dados estejam formatados corretamente antes de trabalhar com eles. Isso inclui tratar possíveis valores nulos e caracteres especiais presentes no arquivo.

## Veja também

- Documentação oficial do pacote "encoding/csv": https://golang.org/pkg/encoding/csv/
- Exemplos de uso de CSV em Go: https://gobyexample.com/reading-files
- Tutorial em vídeo sobre como trabalhar com CSV em Go: https://www.youtube.com/watch?v=6hFx5yL7bW0