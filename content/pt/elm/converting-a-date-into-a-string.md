---
title:    "Elm: Convertendo uma data em uma string"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Por que

Quando estamos trabalhando com datas em nossos programas, muitas vezes precisamos convertê-las em formato de string para exibi-las ao usuário ou enviar por meio de chamadas de API. A linguagem de programação Elm oferece uma maneira fácil e eficiente para realizar essa conversão. Neste artigo, vamos explorar como converter uma data em formato de string usando Elm.

## Como fazer

Para converter uma data em uma string em Elm, podemos usar a função `Date.toString`. Ela recebe como parâmetros uma string de formato e um valor de data. Aqui está um exemplo de como usá-la:

```Elm
import Date

myDate = Date.fromYMD 2020 07 26

stringDate = Date.toString "dd/MM/yyyy" myDate

-- saída: "26/07/2020"
```

No exemplo acima, primeiro importamos o módulo `Date` para ter acesso à função `toString`. Então, criamos uma variável `myDate` com a data que queremos converter, usando a função `Date.fromYMD` para criar a data com o ano, mês e dia especificados. Em seguida, usamos a função `toString` para converter a data na variável `stringDate`, especificando o formato desejado como "dd/MM/yyyy". A partir disso, obtemos a string "26/07/2020" como resultado.

## Mergulho Profundo

Uma das coisas interessantes sobre a função `Date.toString` é que ela aceita diferentes formatos de string como parâmetro. Por exemplo, podemos usar "yyyy-MM-dd" para obter a data no formato "2020-07-26" ou "MMM dd, yyyy" para obter "Jul 26, 2020". Além disso, podemos adicionar opções extras ao formato, como "EEEE" para obter o dia da semana completo ("Sunday") ou "EEE" para obter a forma abreviada ("Sun"). Você pode conferir a lista completa de opções de formato na documentação oficial da linguagem Elm.

Além disso, podemos usar a função `Date.toIsoString` para obter a data em formato ISO 8601, que é amplamente utilizado em APIs. Essa função também aceita um objeto de configuração como parâmetro, permitindo-nos especificar opções adicionais, como se devemos incluir ou não o fuso horário na string.

## Veja também

Aqui estão alguns links úteis para saber mais sobre como converter datas em strings usando Elm:

- Documentação oficial da função `Date.toString`: https://package.elm-lang.org/packages/elm/time/latest/Date#toString
- Documentação oficial da função `Date.toIsoString`: https://package.elm-lang.org/packages/elm/time/latest/Date#toIsoString
- Formatos de strings suportados pela função `Date.toString`: https://package.elm-lang.org/packages/elm/time/latest/Time#Format