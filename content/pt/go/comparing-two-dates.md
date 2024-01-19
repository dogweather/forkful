---
title:                "Comparando duas datas"
html_title:           "C#: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Comparar duas datas é uma prática comum no desenvolvimento de software para determinar qual é anterior, posterior ou se são iguais. Isso é útil para ordenar eventos, calcular a duração de um período, verificar prazos, entre outros.

## Como fazer:

Com o pacote de tempo do Go, é simples e direto. Aqui estão alguns exemplos:

```Go
package main
import (
"time"
"fmt"
)

func main() {
	primeiraData := time.Date(2000, 1, 1, 0, 0, 0, 0, time.UTC)
	segundaData := time.Date(2000, 1, 1, 0, 0, 0, 0, time.UTC)

	if primeiraData.Before(segundaData) {
		fmt.Println("Primeira data é anterior à segunda data.")
	}

	if segundaData.After(primeiraData) {
		fmt.Println("Segunda data é depois da primeira data.")
	}

	if segundaData.Equal(primeiraData) {
		fmt.Println("Ambas as datas são iguais.")
	}

}
```

A saída:

```Go
Ambas as datas são iguais.
```
Os métodos `Before`, `After` e `Equal` do pacote de tempo facilitam a comparação de datas.

## Deep Dive

As comparações de data em Go são diretas devido à implementação eficiente do pacote Time. Em versões anteriores de Go (anterior a 1.4), tínhamos que fazer um pouco mais de trabalho para comparar datas. Porém, desde a versão 1.4, o pacote de tempo ganhou esses úteis métodos que simplificam enormemente nosso trabalho.

Existem alternativas, como criar seus próprios métodos para comparação de datas ou usar pacotes de terceiros, mas essas abordagens são geralmente mais complicadas e não oferecem benefícios tangíveis em relação à solução nativa.

Além disso, a hora em Go é armazenada internamente como nanossegundos desde a época Unix, facilitando e agilizando a comparação.

## Veja Também

Para mais informações sobre o pacote de tempo em Go, consulte os seguintes links:

- Documentação oficial: https://golang.org/pkg/time/
- Go por exemplo: https://gobyexample.com/time
- Fórum de discussão Go: https://forum.golangbridge.org/ 
- Go Playground: https://play.golang.org/