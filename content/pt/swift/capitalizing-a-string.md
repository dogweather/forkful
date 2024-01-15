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

## Por que "Capitalizar" uma string?

Capitalizar uma string é um processo comumente utilizado em programação para tornar as letras maiúsculas de uma palavra ou frase. Isso pode ser útil para apresentar dados de forma mais legível ou criar um padrão específico de capitalização.

## Como Fazer

A linguagem de programação Swift oferece diversas opções para capitalizar strings de forma eficiente. Vamos dar uma olhada em três métodos diferentes para alcançar o mesmo objetivo.

```Swift
let myString = "hello world"

// método 1: utilizando o método uppercased()
let capitalizedString = myString.uppercased()
print(capitalizedString) // imprime "HELLO WORLD"

// método 2: utilizando o método capitalized()
let otherCapitalizedString = myString.capitalized
print(otherCapitalizedString) // imprime "Hello World"

// método 3: utilizando um loop e a função uppercased() nos caracteres individuais da string
var finalCapitalizedString = ""
for character in myString {
    finalCapitalizedString += String(character).uppercased()
}
print(finalCapitalizedString) // imprime "HELLO WORLD"
```

Como podemos ver, existem diferentes abordagens para capitalizar uma string. O método escolhido dependerá do contexto e dos requisitos do seu projeto.

## Mergulho Profundo

Além dos métodos mencionados acima, a linguagem Swift também oferece a opção de criar uma extensão para o tipo String, adicionando um novo método personalizado para capitalizar facilmente uma string. Isso pode ser feito da seguinte forma:

```Swift
extension String {
    func capitalize() -> String {
        return self.capitalized
    }
}

// agora podemos utilizar o método capitalize() diretamente em uma string
let myString = "hello world"
print(myString.capitalize()) // imprime "Hello World"
```

Além disso, é importante ter em mente que a capitalização de uma string é sensível a idiomas. Em Swift, podemos utilizar o enum `Casing` do framework `Foundation` para especificar o idioma desejado ao capitalizar uma string. Por exemplo:

```Swift
let myString = "é por isso que precisamos de idiomas específicos"
print(myString.capitalized(with: .init()) // imprime "É Por Isso Que Precisamos De Idiomas Específicos"
```

## Veja também

- Documentação oficial da linguagem Swift sobre a manipulação de strings: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html

- Guia de referência rápida para o método `String.capitalized()`: https://www.hackingwithswift.com/tips/swift-3-how-to-capitalize-the-first-letter-of-a-string