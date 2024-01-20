---
title:                "Convertendo uma data em uma string"
html_title:           "C++: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Convertendo uma data em string no Go

## O Que & Por Que?
Converter uma data em string significa transformar um objeto do tipo data em texto. Programadores fazem isso para facilitar a exibição, armazenamento e manipulação de datas.

## Como Fazer:
Aqui, temos um código simples que demonstra como converter uma data em string em Go.

```Go
pacote main

import ( 
	"fmt"
	"time"
)

func main() {
	agora := time.Now()
	dataComoString := agora.Format("2006-01-02")
	fmt.Println(dataComoString)
}
```
Quando você executa isso, vai receber um output assim:

```Go
2022-03-10
```
Este é o formato yyyy-mm-dd, comum para os padrões de data internacionais.

## Mergulho Profundo

No Go, a função Format de time.Time é usada para converter uma data em string. A cadeia mágica de data "2006-01-02" é um guia para o formato desejado que você quer que sua data apareça.

Essa sintaxe aparentemente aleatória representa a data de referência do Go. Para ser mais específico, é o "detalhe zero" de como a data/hora é registrada internamente no Go. 

Existem algumas alternativas para converter uma data em string no Go, por exemplo, poderia-se usar a função Sprintf do pacote fmt:

```Go
dataComoString := fmt.Sprintf("%d-%02d-%02d", agora.Year(), agora.Month(), agora.Day())
```

## Veja Também:

Para mais informações, aqui estão alguns links úteis:

- Documentação oficial do Go para Pacote de Tempo: https://pkg.go.dev/time
- Um tutorial útil sobre datas e horários em Go: https://yourbasic.org/golang/format-parse-string-time-date-example/

Aprender a manejar datas é fundamental para qualquer desenvolvedor. Espero que este rápido guia tenha te ajudado a entender melhor como converter datas em strings no Go.