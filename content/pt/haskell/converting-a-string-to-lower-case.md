---
title:    "Haskell: Convertendo uma string para letras minúsculas"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que

Existem várias situações em que pode ser útil converter uma string para letras minúsculas em um programa Haskell. Isso pode facilitar a comparação de strings, a formatação do texto e a garantia de que o código funcione corretamente em diferentes sistemas operacionais.

## Como fazer

Para converter uma string para letras minúsculas em Haskell, podemos utilizar a função `map` em conjunto com a função `toLower` do módulo `Data.Char`. Abaixo está um exemplo de código que faz a conversão de uma string para letras minúsculas e imprime o resultado.

```Haskell
import Data.Char

main = do
    let string = "Exemplo DE sTRING"
    let novaString = map toLower string
    putStrLn novaString
```

A saída deste código será: `exemplo de string`. Podemos ver que a função `map` aplicou a função `toLower` em cada caractere da string e retornou uma nova string com todas as letras minúsculas.

## Aprofundando-se

Em Haskell, as strings são apenas listas de caracteres. Portanto, quando usamos a função `map` para percorrer cada caractere, estamos na verdade aplicando a função a todos os elementos da lista, que no caso são os caracteres da string.

Outra forma de converter uma string para letras minúsculas é usando a função `toLower` diretamente em cada caractere da string, ao invés de usar a função `map`. Porém, isso exigiria um pouco mais de conhecimento sobre manipulação de listas em Haskell.

A conversão para letras minúsculas também pode ser feita com o uso de expressões regulares, utilizando a função `subRegex` do módulo `Text.Regex`. Porém, essa abordagem é mais complexa e pode não ser tão eficiente quanto as funções mencionadas anteriormente.

## Veja também

- [Documentação do módulo Data.Char em Haskell](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html)
- [Tutorial sobre strings em Haskell](https://www.tutorialspoint.com/haskell/haskell_strings.htm)
- [Documentação do módulo Text.Regex em Haskell](https://hackage.haskell.org/package/regex-compat-0.95.1/docs/Text-Regex.html)