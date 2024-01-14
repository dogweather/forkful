---
title:                "Gleam: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Por que Comparar Duas Datas em Gleam?

Ao escrever programas em Gleam, às vezes é necessário comparar duas datas. Isso pode ser útil para verificar qual data é mais recente ou para saber se uma data está no passado ou no futuro. Felizmente, Gleam oferece uma maneira simples e eficiente de realizar essa tarefa.

# Como Comparar Duas Datas

Para comparar duas datas em Gleam, usamos o módulo `Time` e a função `compare_dates`. Nós fornecemos as duas datas que queremos comparar como parâmetros e a função nos retorna um dos seguintes resultados:

- `Order.GreaterThan`: se a primeira data for posterior à segunda data.
- `Order.LessThan`: se a primeira data for anterior à segunda data.
- `Order.EqualTo`: se as duas datas forem iguais.

Veja um exemplo de como usar a função `compare_dates`:

```Gleam
import Time

let date1 = Time.parse("2021-05-10", "yyyy-MM-dd")
let date2 = Time.parse("2021-04-15", "yyyy-MM-dd")
let result = Time.compare_dates(date1, date2)
```

Neste exemplo, a data `date1` é maior que a data `date2`, então o valor de `result` será `Order.GreaterThan`.

# Profundidade da Comparação de Datas

Embora a função `compare_dates` seja útil para a maioria das comparações de datas, é importante entender como ela funciona por dentro. O módulo `Time` usa o padrão ISO-8601 para representar datas e horas em Gleam, o que facilita a comparação entre elas. No entanto, é importante ter cuidado ao usar datas de outros formatos, pois isso pode levar a resultados inesperados.

Além disso, é importante observar que a função `compare_dates` compara as datas em relação à sua representação numérica, sem levar em consideração outros fatores, como horário ou fuso horário. Portanto, é sempre uma boa prática converter as datas para o mesmo formato antes de compará-las.

# Veja Também

Aqui estão alguns links úteis para continuar aprendendo sobre como trabalhar com datas em Gleam:

- Documentação oficial do módulo `Time`: https://gleam.run/modules/time.html
- Tutorial sobre como usar datas em Gleam: https://gleam.run/book/times.html
- Repositório oficial Gleam no GitHub: https://github.com/gleam-lang/gleam