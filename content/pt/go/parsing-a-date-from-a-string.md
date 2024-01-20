---
title:                "Analisando uma data a partir de uma string"
html_title:           "PowerShell: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê? 

Fazer o "parse" de uma data de uma string significa transformá-la em um formato utilizável no Go, como um objeto time.Time. Isto é útil porque permite manipular e comparar datas de maneira eficiente e precisa.

## Como fazer:

Vamos direto ao código.

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	const baseFormat = "2006-01-02"
	str := "2021-04-22"
	
	t, err := time.Parse(baseFormat, str)
	if err != nil {
		fmt.Println("erro:", err)
		return
	}
	
	fmt.Println(t)
}
```
Output:

```Go
2021-04-22 00:00:00 +0000 UTC
```
Neste exemplo, a string "2021-04-22" é convertida para um objeto do time.Time. Se ocorrer um erro durante o parse, ele será impresso.

## Deep Dive:

O Go tem uma abordagem única para parsing de datas. Ele usa uma data de referência (Mon Jan 2 15:04:05 MST 2006) e a formata para representar a string de entrada. Por exemplo, para tratar uma data no formato "YYYY-MM-DD", formatamos a data de referência para "2006-01-02".

Existe uma alternativa ao uso de time.Parse, que é time.ParseInLocation. Isto permite especificar um fuso horário particular durante o parsing.

Além disso, ao fazer o parse do fuso horário, se nenhuma zona for especificada, ele usará UTC. Para especificar o parse do fuso horário, você pode usar 'Z0700' (RFC 822), 'Z07:00' (ISO 8601) ou o nome do fuso horário. 

## Ver Também:

Para aprender mais sobre o "time package" em Go e a data de referência, recomendo estes links:
- Documentação oficial de Time Package: [https://golang.org/pkg/time/](https://golang.org/pkg/time/)
- Análise aprofundada do parsing de data/time em Go: [https://yourbasic.org/golang/format-parse-string-time-date-example/](https://yourbasic.org/golang/format-parse-string-time-date-example/) 
- Strings de layout no tempo do Go: [https://flaviocopes.com/go-date-time-format/](https://flaviocopes.com/go-date-time-format/)