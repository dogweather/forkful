---
title:                "Swift: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair substrings em Swift?

Extrair substrings é uma habilidade importante na programação em Swift, pois permite que você manipule com eficiência as cadeias de caracteres (strings). Isso pode ser útil ao analisar dados ou ao formatar uma saída para exibição.

## Como fazer

Para extrair uma substring em Swift, você pode usar o método `substring` ou o operador de fatiamento `[]`. Aqui está um exemplo usando o método `substring`:

```Swift
let nome = "Maria Silva"
let sobrenome = nome.substring(from: 6)
print(sobrenome)
```

Isso resultará na saída "Silva", já que o método `substring` extrai as letras a partir do índice especificado (neste caso, 6) até o final da string.

Também é possível usar o operador de fatiamento `[]` para extrair substrings. Aqui está um exemplo:

```Swift
let nomeCompleto = "João da Silva"
let sobrenome = nomeCompleto[8...]
print(sobrenome)
```

Isso resultará na saída "Silva", pois o operador de fatiamento extrai as letras do índice 8 até o final da string.

## Aprofundando-se

Ao trabalhar com substrings, é importante ter em mente que elas são representações da string original, e qualquer modificação feita nelas não afetará a string original. Por exemplo, ao usar o método `substring`, uma nova string é criada a partir da string original, mas quando se trabalha com o operador de fatiamento, é criada uma visualização da substring na string original.

Além disso, é importante entender o uso de índices ao extrair substrings. Em Swift, o primeiro caractere de uma string tem o índice 0, e o último caractere tem o índice `string.count - 1`.

## Veja também

- [Documentação oficial do Swift sobre substrings](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID277)
- [Tutorial sobre manipulação de strings em Swift](https://www.appcoda.com/swift-string/)
- [Fórum da Swift em Português](https://swift.com.br/forum/)