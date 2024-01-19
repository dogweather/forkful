---
title:                "Convertendo uma string para minúsculas"
html_title:           "Fish Shell: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Converter uma string para minúscula é simplesmente mudar todas as letras maiúsculas em uma string para letras minúsculas. Os programadores fazem isso para normalizar os dados, permitindo comparações mais precisas e simplificadas entre strings.

## Como Fazer:

Vamos usar o método **lowercased()** da classe String em Swift.

```Swift
let textoOriginal = "OLÁ, MUNDO!"
let textoMinúsculo = textoOriginal.lowercased()
print(textoMinúsculo) 
// Saída: "olá, mundo!"
```

## Aprofundamento

A conversão de string para minúsculas tem sido uma prática comum por muitos anos, principalmente porque permite um tratamento uniforme das entradas, independentemente de como os dados foram inicialmente inseridos.

Em Swift, além do método **lowercased()**, você pode usar a função **localizedLowercase** que retorna a representação em minúsculas da string de acordo com as regras do local atual.

```Swift
let textoOriginal = "OLÁ, MUNDO!"
let textoMinúsculo = textoOriginal.localizedLowercase
print(textoMinúsculo)
// Saída: "olá, mundo!"  
```

Agora, se falarmos sobre detalhes de implementação, o método **lowercased()** percorre a string e para cada caracter verifica se é uma letra maiúscula. Se for, ele a substitui pela versão minúscula.

## Veja Também:

1. [String Manipulation in Swift](https://developer.apple.com/documentation/swift/string)
2. [Swift Documentation: lowercased() function](https://developer.apple.com/documentation/swift/string/2290197-lowercased)
3. [Swift Documentation: localizedLowercase](https://developer.apple.com/documentation/swift/string/1641535-localizedlowercase)

Lembre-se de que, ao manipular strings, é importante entender bem as funções que está a usar para evitar erros inesperados. E sempre que ficar em dúvida, consulte a documentação oficial do Swift fornecida pela Apple.