---
title:                "Analisando uma data a partir de uma string"
date:                  2024-01-20T15:36:18.197132-07:00
simple_title:         "Analisando uma data a partir de uma string"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Transformar uma data de uma string (sequência de caracteres) em uma estrutura de data no Go é como transformar uma frase em uma ideia clara e organizada. Programadores fazem isso para manipular, formatar e realizar cálculos com datas, o que é essencial em muitas aplicações, como agendamentos e registros temporais.

## Como Fazer:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// formato base (ANSIC): Mon Jan _2 15:04:05 2006
	layout := "2006-01-02 15:04:05"
	str := "2023-03-14 11:45:26"

	// Parse a data a partir da string
	t, err := time.Parse(layout, str)
	if err != nil {
		fmt.Println("Erro ao analisar data:", err)
		return
	}

	// Exibe a data e hora
	fmt.Println("Data Analisada:", t)
}
```

Saída de exemplo:
```
Data Analisada: 2023-03-14 11:45:26 +0000 UTC
```

## Em Detalhe:

Historicamente, lidar com datas sempre foi um desafio na programação. Go facilita esse processo com o pacote `time`, que segue o específico padrão de layout para interpretar e formatar datas. Essencialmente, utilizamos a data de referência `Mon Jan 2 15:04:05 MST 2006` para criar o layout que o método `Parse` irá entender. Alternativas incluem usar bibliotecas de terceiros, como `dateparse`, que podem oferecer análise de datas mais flexível.

A implementação no Go é poderosa pois lida com fusos horários e considera especificidades de calendário. No entanto, formatos incoerentes ou incorretos resultarão em erros, destacando a importância de se utilizar o layout de forma precisa.

## Veja Também:

- Documentação oficial do pacote `time` no Go: [pkg.go.dev/time](https://pkg.go.dev/time)
- Artigo sobre padrões de tempo e data em Go: [yourbasic.org/golang/format-parse-string-time-date-example](https://yourbasic.org/golang/format-parse-string-time-date-example)
- Pacote `dateparse` para Go: [github.com/araddon/dateparse](https://github.com/araddon/dateparse)
