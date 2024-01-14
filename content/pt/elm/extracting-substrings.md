---
title:                "Elm: Extraindo subcadeias"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair substrings em Elm?

Extrair substrings é um processo bastante comum em programação. Pode ser necessário quando você deseja obter partes específicas de uma string maior, ou quando precisa fazer manipulação de dados mais complexa. Em Elm, a manipulação de strings é feita de forma segura e imutável, o que torna o processo de extrair substrings ainda mais vantajoso.

## Como fazer

Para extrair substrings em Elm, usamos a função `slice`, que trabalha com duas posições de índice da string original. Os dois índices são fornecidos como argumentos da função, e a substring resultante é retornada como um novo valor. Vejamos um exemplo:

```Elm
slice 2 5 "banana" -- retorna "nan"
```

No exemplo acima, o primeiro argumento `2` corresponde à posição inicial da substring, enquanto o segundo argumento `5` corresponde à posição final. É importante notar que o índice começa em zero e a posição final não é incluída na substring resultante.

Podemos usar também valores negativos para os índices, que são contados a partir do final da string. Por exemplo:

```Elm
slice -3 -1 "banana" -- retorna "an"
```

Não se preocupe se os índices fornecidos forem maiores que o tamanho da string original. Nesse caso, a função simplesmente retorna a string original.

```Elm
slice 0 20 "banana" -- retorna "banana"
```

## Deep Dive

Além da função `slice`, Elm também oferece a função `substring`, que possui uma assinatura ligeiramente diferente:

```Elm
substring : Int -> Int -> String -> String
```

Como podemos ver, esta função requer um argumento adicional - a string original. No entanto, ela possui a mesma funcionalidade da função `slice`.

Além disso, é importante mencionar que as funções `slice` e `substring` são apenas alguns exemplos de como podemos extrair substrings em Elm. Existem outras funções disponíveis, como `left`, `right` e `mid`, que também podem ser úteis em diferentes situações.

## Veja também

- Documentação oficial Elm sobre funções de strings: https://package.elm-lang.org/packages/elm/core/latest/String
- Tutorial Elm: Manipulando strings de forma segura: https://dev.to/kmetter/tutorial-elm-manipulating-strings-safely-1ffc
- Exemplos de uso de funções de strings em Elm: https://www.programming-idioms.org/idiom/61/substring-with-boundaries/780/elm