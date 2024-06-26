---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:00:13.180446-07:00
description: "Como Fazer: Go oferece um suporte robusto para an\xE1lise de datas e\
  \ hor\xE1rios atrav\xE9s do pacote `time`. A chave \xE9 compreender o formato de\
  \ data de refer\xEAncia\u2026"
lastmod: '2024-03-13T22:44:46.071320-06:00'
model: gpt-4-0125-preview
summary: "Go oferece um suporte robusto para an\xE1lise de datas e hor\xE1rios atrav\xE9\
  s do pacote `time`."
title: Analisando uma data a partir de uma string
weight: 30
---

## Como Fazer:
Go oferece um suporte robusto para análise de datas e horários através do pacote `time`. A chave é compreender o formato de data de referência de Go: `Seg Jan 2 15:04:05 MST 2006`, que você usa para indicar ao Go como interpretar a string de entrada. Aqui está um exemplo rápido para começar:

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Exemplo de string de data
	dateStr := "2023-04-12 14:45:00"
	
	// Definir o layout/formato da string de data de entrada
	// Este layout diz ao Go que espera um ano, seguido por um mês, 
	// depois um dia, hora, minuto e, finalmente, segundo
	layout := "2006-01-02 15:04:05"
	
	// Analisar a string de data de acordo com o layout
	parsedDate, err := time.Parse(layout, dateStr)
	if err != nil {
		fmt.Println("Erro ao analisar data:", err)
		return
	}
	
	// Exibir a data analisada
	fmt.Println("Data Analisada:", parsedDate)
}
```

Quando você executar esse código, vai obter:

```
Data Analisada: 2023-04-12 14:45:00 +0000 UTC
```

Note como a string `layout` usa os valores da data de referência para especificar o formato da string de entrada. Ajuste o `layout` para combinar com o formato de suas datas de entrada.

## Aprofundando
O design da análise de data e hora em Go é único, utilizando uma data de referência específica (`Seg Jan 2 15:04:05 MST 2006`). Esse método, em vez de usar especificadores de formato mais convencionais (como `AAAA` para ano), foi escolhido pela legibilidade e facilidade de uso, aproveitando um formato mais baseado em exemplos.

Embora isso possa inicialmente parecer incomum para programadores acostumados com outras línguas, muitos acham mais intuitivo após um breve período de ajuste. Para aplicações que exigem manipulação de data mais complexa ou formatos não diretamente suportados pelo pacote `time` do Go, bibliotecas de terceiros como `github.com/jinzhu/now` podem oferecer funcionalidades adicionais. Contudo, para a maioria das aplicações padrão, as capacidades integradas do Go são robustas, performáticas e idiomáticas, personificando a filosofia do Go de simplicidade e clareza.
