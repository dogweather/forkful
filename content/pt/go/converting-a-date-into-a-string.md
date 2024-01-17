---
title:                "Convertendo uma data em uma string"
html_title:           "Go: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Converter uma data em uma string é um processo fundamental para os programadores em Go, pois permite que as datas sejam formatadas de acordo com as preferências dos usuários. Além disso, a conversão de uma data em uma string torna mais fácil a manipulação e a exibição de datas em diferentes formatos.

## Como fazer:
```
Go func main() {
    // Definir uma data
    data := time.Date(2021, time.December, 25, 12, 00, 00, 00, time.UTC)
    
    // Converter a data em uma string no formato "dia/mês/ano"
    dataString := data.Format("02/01/2006")
    
    fmt.Println(dataString) // Output: 25/12/2021
    
    // Converter a data em uma string no formato "dia da semana, dia de mês de ano"
    dataString2 := data.Format("Monday, 02 January 2006")
    
    fmt.Println(dataString2) // Output: Saturday, 25 December 2021
}
```

## Mergulho Profundo:
Converter datas em strings é uma funcionalidade comum em linguagens de programação, permitindo que os desenvolvedores controlem como as datas são formatadas e exibidas aos usuários. Em Go, a função `Date()` do pacote `time` é usada para criar uma data com valores específicos, e a função `Format()` é usada para converter essa data em uma string com o layout desejado. Existem outras opções de formatação de datas, como o pacote `strconv`, mas a função `Format()` do pacote `time` é a mais simples e direta.

## Ver também:
- Documentação do pacote `time`: https://golang.org/pkg/time/
- Tutorial sobre formatação de datas em Go: https://www.calhoun.io/using-the-time-package-in-go/