---
title:                "Go: Obtendo a data atual."
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual em programação é importante

Trabalhar com datas é um aspecto crucial da programação em qualquer linguagem. É importante ser capaz de obter a data atual em seu código para realizar diversas tarefas, como registrar eventos, calcular durações ou gerar relatórios. Neste artigo, vamos mostrar como fazer isso em Go.

## Como obter a data atual em Go

Existem várias maneiras de obter a data atual em Go, mas a forma mais simples é usando a função `time.Now()`. Por exemplo:

```Go
dataAtual := time.Now()
fmt.Println(dataAtual)
```
A saída desse código será algo como `2021-11-01 10:08:04.62391 +0000 UTC m=+0.000121771`.

Se você quiser formatar a data, pode usar o método `Format()` da struct `Time`. Por exemplo:

```Go
dataFormatada := dataAtual.Format("02/01/2006")
fmt.Println(dataFormatada)
```
A saída desse código será `01/11/2021`, seguindo o formato de dia/mês/ano.

Outra forma de obter a data atual em um formato especificado é usando o pacote `time` com constantes, como `time.RFC822` ou `time.RFC3339`. Por exemplo:

```Go
dataRFC822 := dataAtual.Format(time.RFC822)
dataRFC3339 := dataAtual.Format(time.RFC3339)
fmt.Println(dataRFC822)
fmt.Println(dataRFC3339)
```
A saída desses códigos será, respectivamente, `01 Nov 21 10:08 UTC` e `2021-11-01T10:08:04Z`.

## Profundando no código

Por baixo dos panos, a função `time.Now()` utiliza o relógio do sistema para obter a data e hora atuais. O retorno dessa função é um objeto do tipo `Time`, que representa uma data e hora específicas e permite realizar diversas operações, como adicionar ou subtrair tempo, comparar com outras datas, entre outras.

Vale mencionar também que a função `time.Now()` sempre retorna a hora no fuso horário UTC. Se você quiser obter a hora no fuso horário do seu sistema, pode usar o método `Local()` da struct `Time`.

## Veja também

- [Documentação oficial do pacote time em Go](https://pkg.go.dev/time)
- [Tutorial de datas e horas em Go](https://www.golangprograms.com/golang-get-current-date-time.html)
- [Exemplo de código em Go para formatar datas](https://www.golangprograms.com/go-program-to-format-current-date-time-in-built-common-formats.html)