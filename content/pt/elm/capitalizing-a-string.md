---
title:    "Elm: Capitalizando uma string."
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Por que capitalizar uma string em Elm?

Se você é novo no mundo da programação funcional ou no idioma Elm, pode estar se perguntando por que alguém precisaria capitalizar uma string em primeiro lugar. A resposta é simples: às vezes, temos que trabalhar com dados fornecidos por usuários ou outras fontes, e esses dados podem não estar formatados da maneira que desejamos. Capitalizar uma string pode ser útil em situações como essa, para garantir que nossos dados estejam padronizados e facilmente legíveis.

## Como fazer em Elm

Agora que entendemos a importância de capitalizar uma string, vamos dar uma olhada em como fazer isso em Elm. Usando a função `toUpper`, podemos facilmente converter uma string para letras maiúsculas. Por exemplo, se tivermos a string "olá", podemos capitalizá-la da seguinte forma: 

```Elm
String.toUpper "olá"

-- Output: "OLÁ"
```

Mas e se quisermos apenas capitalizar a primeira letra de uma string? Para fazer isso, podemos usar a função `capitalize` do módulo `String`, juntamente com a função `toUpper` para garantir que a primeira letra esteja em maiúscula e o restante em minúsculas. Veja um exemplo:

```Elm
String.capitalize "elm"

-- Output: "Elm"
```

## Mergulho profundo

Agora que vimos como simples é capitalizar uma string em Elm, vamos dar um mergulho mais profundo para entender como essas funções funcionam. A função `capitalize` é na verdade uma composição de funções, ou seja, ela combina várias funções para obter o resultado desejado. Essas funções incluem `toUpper`, `toLower` e `append`. Usar a função `append` nos permite juntar a primeira letra da string em maiúscula com o restante da string em minúsculas, obtendo assim o resultado desejado.

## Veja também

- [Documentação da função toUpper em Elm](https://package.elm-lang.org/packages/elm/core/latest/String#toUpper)
- [Documentação da função capitalize em Elm](https://package.elm-lang.org/packages/elm/core/latest/String#capitalize)
- [Tutorial sobre funções em Elm](https://guide.elm-lang.org/functions/)