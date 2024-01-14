---
title:    "Gleam: Comparando duas datas"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Por que comparar duas datas em Gleam?

Comparar datas é uma tarefa comum em programação, especialmente quando se trata de criar aplicativos que lidam com dados temporais. Em Gleam, há uma maneira fácil e eficiente de comparar duas datas, o que pode facilitar muito o desenvolvimento de seus projetos. Neste post, vamos mostrar como realizar essa comparação em Gleam e também fornecer informações mais aprofundadas sobre o assunto.

## Como comparar duas datas em Gleam

Para comparar duas datas em Gleam, usamos o módulo `Date.Comparison`, que fornece funções para comparar datas e determinar se uma é anterior, igual ou posterior a outra. Vamos dar uma olhada em um exemplo simples:

```
Gleam
import Date

let data1 = Date.from_iso("2020-10-10")
let data2 = Date.from_iso("2020-05-05")

Date.Comparison.compare(data1, data2)
```

Neste exemplo, usamos a função `from_iso` para criar duas datas a partir de strings no formato ISO. Em seguida, usamos a função `compare` para comparar as datas e obtemos como resultado o valor `GreaterThan`, indicando que a `data1` é posterior à `data2`.

O módulo `Date.Comparison` também possui outras funções úteis, como `eq` para verificar se duas datas são iguais e `lt` para determinar se uma data é anterior à outra.

## Mergulho profundo: comparando datas com precisão

Ao comparar datas em Gleam, é importante ter em mente que as comparações são feitas com base na precisão de um dia. Isso significa que, se as datas tiverem a mesma precisão, elas serão verificadas quanto a igualdade ou ordem. No entanto, se houver uma diferença na precisão, apenas a data mais precisa será levada em consideração para a comparação.

Por exemplo, se tivermos duas datas, uma com precisão de dia e outra com precisão de milissegundos, a comparação será feita apenas com base no dia e a diferença nos milissegundos será ignorada.

Além disso, devemos estar atentos às diferenças no fuso horário ao comparar datas, pois isso também pode afetar os resultados.

## Veja também

- [Documentação oficial do módulo Date.Comparison](https://gleam.run/modules/gleam/datetime/1.0.0/Date.Comparison.html)
- [Post sobre manipulação de datas em Gleam](https://example.com)

# Veja também