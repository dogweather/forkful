---
title:                "Trabalhando com CSV"
date:                  2024-01-19
html_title:           "Bash: Trabalhando com CSV"
simple_title:         "Trabalhando com CSV"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/working-with-csv.md"
---

{{< edit_this_page >}}

## O Que & Por Que?

CSV (Valores Separados por Vírgula) é um formato de arquivo usado para armazenar dados tabulares. Programadores trabalham com CSV pois é simples, amplamente suportado e facilita a interoperabilidade entre diferentes sistemas.

## Como Fazer:

### Lendo CSV
```Go
package main

import (
    "encoding/csv"
    "fmt"
    "strings"
)

func main() {
    csvData := `nome,idade,país
                João,24,Brasil
                Ana,35,Portugal`
    
    reader := csv.NewReader(strings.NewReader(csvData))
    for {
        line, err := reader.Read()
        if err != nil {
            break
        }
        fmt.Println(line) // Output: [nome idade país] então [João 24 Brasil] e assim por diante
    }
}
```

### Escrevendo CSV
```Go
package main

import (
    "encoding/csv"
    "os"
)

func main() {
    records := [][]string{
        {"nome", "idade", "país"},
        {"Carlos", "29", "Angola"},
        {"Beatriz", "22", "Brasil"},
    }

    file, err := os.Create("output.csv")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    writer := csv.NewWriter(file)
    defer writer.Flush()

    for _, record := range records {
        if err := writer.Write(record); err != nil {
            panic(err)
        }
    }
}
```

## Mergulho Profundo

Trabalhar com CSV não é novidade. Foi criado na década de 1970 e desde então se tornou padrão para troca de dados. Alternativas modernas incluem JSON e XML, mas CSV permanece popular devido à sua simplicidade. Ao usar CSV em Go, detalhes de implementação como tratamento de caracteres especiais e codificação de caracteres são manipulados pela biblioteca `encoding/csv`.

## Veja Também

- Documentação oficial do Go para CSV: https://pkg.go.dev/encoding/csv
- Go by Example, trabalhando com arquivos CSV: https://gobyexample.com/reading-files
- Tutorial detalhado de CSV em Go: https://www.thepolyglotdeveloper.com/2017/03/parse-csv-data-go-programming-language/
