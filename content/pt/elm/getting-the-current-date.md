---
title:    "Elm: Obtendo a data atual"
keywords: ["Elm"]
---

{{< edit_this_page >}}

# Por que usar Elm para obter a data atual?

Se você é um programador em busca de uma linguagem de programação funcional e fácil de aprender, a resposta é simples: Elm. Com Elm, você pode facilmente obter a data atual em seu código, sem a necessidade de bibliotecas ou pacotes adicionais. Além disso, Elm é conhecida por sua sintaxe concisa e robusta, tornando a tarefa ainda mais fácil.

## Como fazer isso em Elm?

Para obter a data atual em Elm, basta usar a função `Time.now` e em seguida, converter para o formato que desejar. Por exemplo, para obter a data atual no formato ano-mês-dia, você poderia fazer o seguinte:

```Elm
import Time exposing (..)

getDate : String
getDate =
    Time.now |> Time.toYearMonthDay |> toString
```

E ao chamar a função `getDate`, o resultado será algo como `2021-09-20`. Simples, não é?

## Profundidade na obtenção da data atual em Elm

Para aqueles que gostam de mergulhar um pouco mais fundo, é possível entender como exatamente a função `Time.now` funciona em Elm. Na verdade, por trás dos panos, essa função utiliza o objeto `Date` do JavaScript para obter a data atual e, em seguida, o converte para um formato Elm amigável.

Então, por que não simplesmente manipular o objeto `Date` diretamente? A resposta é que, ao usar a função `Time.now`, você garante que seu código seja mais seguro e livre de erros, já que o objeto `Date` possui alguns comportamentos peculiares e propenso a bugs.

Além disso, Elm possui uma documentação extremamente detalhada e acessível para aqueles que desejam explorar mais a fundo o funcionamento das funções de data e tempo.

# Veja também

- [Documentação oficial do Elm sobre tempo e data](https://guide.elm-lang.org/architecture/effects/time.html)
- [Guia de sintaxe concisa do Elm](https://elm-lang.org/docs/syntax)
- [Curso de Elm para iniciantes](https://www.youtube.com/watch?v=ddUfxR5sGFo&t=1624s)