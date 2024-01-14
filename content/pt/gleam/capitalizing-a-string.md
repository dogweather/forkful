---
title:    "Gleam: Capitalizando uma string"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string em Gleam?

Ao trabalhar com strings no Gleam, pode ser útil capitalizar a primeira letra de cada palavra em uma frase. Isso pode ser usado para criar títulos elegantes, nomes de variáveis padronizados ou qualquer outra situação em que você precise modificar uma string.

## Como fazer isso em Gleam

Para capitalizar uma string, podemos utilizar a função `String.to_title_case` do módulo `String`, passando a string como argumento. Veja um exemplo:

```Gleam
String.to_title_case("este é um exemplo") // "Este É Um Exemplo"
```

Podemos ver que a primeira letra de cada palavra agora está em maiúsculo. Além disso, podemos utilizar a função `String.to_lower_case` para converter todas as letras para minúsculas e, em seguida, `String.to_upper_case` para converter todas as letras para maiúsculas.

```Gleam
String.to_title_case("exemplO EM MaInUsCuLaS") // "Exemplo Em Maiúsculas"
```

## Mergulho profundo

Ao capitalizar uma string, é importante lembrar que os caracteres especiais não serão alterados. Por exemplo, se uma palavra começar com um caractere especial, ele permanecerá inalterado ao utilizar a função `String.to_title_case`.

Além disso, podemos usar a função `String.words` para dividir uma string em uma lista contendo cada palavra individual.

```Gleam
String.words("dividir esta string") // ["dividir", "esta", "string"]
```

Com essa função e a função `String.to_title_case`, podemos criar nossa própria função personalizada para capitalizar strings em Gleam.

## Veja também

- Documentação oficial de Strings em Gleam: https://gleam.run/docs/std/string
- Mais informações sobre as funções utilizadas neste artigo: https://gleam.run/docs/std/string#title