---
title:                "Comparando duas datas"
html_title:           "Go: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que

Comparar duas datas é uma tarefa comum em programação quando se lida com dados relacionados a tempo, como agendamentos, eventos ou históricos. Entender como fazer essa comparação de maneira eficiente é essencial para um bom desenvolvimento em Go.

## Como fazer

Em Go, é possível comparar datas usando o pacote `time`. Primeiramente, é necessário criar as duas datas que serão comparadas, que podem ser definidas utilizando a função `time.Parse`.

```
data1 := "2021-01-01"
data2 := "2021-01-05"

d1, _ := time.Parse("2006-01-02", data1)
d2, _ := time.Parse("2006-01-02", data2)
```

Com as datas criadas, é possível realizar a comparação utilizando os operadores de comparação: `<`, `<=`, `>`, `>=`, `==` e `!=`. Por exemplo, para verificar se `d1` é anterior a `d2`, basta utilizar o operador `<`.

```
if d1 < d2 {
    fmt.Println("d1 é anterior a d2")
}
```

O resultado esperado do código acima seria "d1 é anterior a d2", já que 01/01/2021 vem antes de 05/01/2021.

## Mergulho Profundo

Quando se trata de comparar datas em Go, é importante entender que as datas são do tipo `time.Time`. Isso significa que elas são armazenadas como um número de nanossegundos desde o Unix epoch (1 de janeiro de 1970). Isso permite uma comparação precisa e eficiente de datas.

Outro detalhe importante é que, ao criar uma data, pode-se especificar o fuso horário que deve ser considerado. Isso pode ser feito passando o local como segundo parâmetro na função `time.Parse`. Por exemplo, para criar uma data em São Paulo, pode-se fazer:

```
localSP, _ := time.LoadLocation("America/Sao_Paulo")
d1, _ := time.ParseInLocation("2006-01-02", data1, localSP)
```

Isso garante que a data `d1` seja criada com base no fuso horário correto, levando em consideração possíveis alterações de horário devido ao horário de verão.

## Veja também

- [Documentação oficial do pacote `time` em Go](https://golang.org/pkg/time/)
- [Exemplos práticos de comparação de datas em Go](https://play.golang.org/p/_OA0SSmMFgS)