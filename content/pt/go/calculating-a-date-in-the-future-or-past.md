---
title:                "Go: Calculando uma data no futuro ou passado."
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Saber como calcular uma data no futuro ou passado é uma habilidade essencial para qualquer programador Go. Isso permite que você crie soluções dinâmicas e precisas para problemas relacionados com datas em suas aplicações.

## Como fazer

Para calcular uma data no futuro ou passado em Go, você precisará utilizar a função `Time.AddDate()`. Essa função recebe três argumentos: anos, meses e dias, e retorna uma data nova acrescida ou subtraída da data original. Veja um exemplo de código abaixo:

```Go
newDate := time.Now().AddDate(1, 2, 3)
fmt.Println(newDate)
```
O código acima irá calcular uma nova data adicionando 1 ano, 2 meses e 3 dias à data atual e imprimi-la no formato de data e hora padrão do sistema.

Algumas coisas importantes a se notar nesta função são:
- Os argumentos podem ser positivos ou negativos dependendo se você quer adicionar ou subtrair da data original.
- A função leva em consideração os anos bissextos e ajusta a data de acordo.
- Você pode utilizar `time.Duration` para adicionar ou subtrair unidades menores de tempo, como horas, minutos e segundos. 

## Mergulho Profundo

Calculando datas no futuro ou passado pode ser mais complexo do que apenas adicionar ou subtrair dias, meses e anos. Uma das coisas que você precisa levar em consideração é o fuso horário. Por exemplo, se você estiver em um lugar com fuso horário diferente do local onde seu código está sendo executado, a data resultante pode ser diferente do esperado.

Além disso, ao lidar com dias úteis, feriados e outras regras de negócio, pode ser necessário utilizar pacotes externos como o `github.com/leekchan/timeutil` ou `github.com/robfig/cron` para ter um maior controle sobre o cálculo de datas.

## Veja também

- [Documentação oficial da função `Time.AddDate()` em Go](https://golang.org/pkg/time/#Time.AddDate)
- [Pacote timeutil](https://github.com/leekchan/timeutil)
- [Pacote cron](https://github.com/robfig/cron)