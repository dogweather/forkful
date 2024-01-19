---
title:                "Capitalizando uma string"
html_title:           "Swift: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Capitalizar uma string em Swift: uma exploração fácil

## O que e por quê?

Capitalizar uma string significa transformar a primeira letra de cada palavra nessa string em maiúscula. Programadores fazem isso para melhorar a apresentação e leitura dos dados textuais, tais como os nomes dos usuários.

## Como fazer:

Podemos fazer isto usando a função `capitalized` que vem embutido em Swift.

Aqui um exemplo de código em Swift que capitaliza uma string:

```Swift
let minhaString = "olá, mundo"
let minhaStringCapitalizada = minhaString.capitalized
print(minhaStringCapitalizada)
```

E a saída deste código será:

```Swift
Olá, Mundo
```

## Mergulhando fundo

Historicamente, a capitalização de strings não era uma preocupação nas primeiras linguagens de programação. Com a crescente interação com o utilizador e necessidade de formatação dos dados textuais, surgiram funções de capitalização.

Existem várias formas de capitalizar uma string em Swift além de `capitalized`. Por exemplo, `uppercased` irá converter todas as letras da string em maiúsculas, e `lowercased` irá converte-las em minúsculas.

A função `capitalized` em Swift faz mais do que apenas capitalizar a primeira letra de cada palavra em uma string. Ela também transforma todas as outras letras em minúscula.

## Ver também

Para mais informações sobre a manipulação de strings em Swift, consulte os seguintes recursos:

- A documentação oficial da Swift da Apple: [The Swift String Manifesto](https://github.com/apple/swift/blob/main/docs/StringManifesto.md)
- Para uma deep dive na classe String do Swift: [Swift by Sundell](https://www.swiftbysundell.com/articles/strings-in-swift/)
- Do Stack Overflow, exemplos de como capitalizar cada palavra de uma string: [Stack Overflow Swift capitalized](https://stackoverflow.com/questions/26306326/swift-capitalization-of-words-in-a-string)  

Espero que isto tenha ajudado a pôr um pouco mais de clareza sobre a capitalização de strings em Swift. Feliz codificação!