---
title:                "Interpolando uma string"
html_title:           "Java: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
A interpolação de strings permite que você insira variáveis ou constantes dentro de uma string. É uma prática eficaz e essencial para a exibição dinâmica de dados nas interfaces do usuário, simplificar a concatenação de strings e melhorar a legibilidade do código.

## Como Fazer:

Vamos inserir uma variável em uma string com Swift:

```Swift
var nome: String = "João"
print("Olá, \(nome)!")
```

O resultado será:

```
Olá, João!
```

Também é possível fazer cálculos dentro das interpolações de string. Por exemplo:

```Swift
var idade: Int = 30
print("Daqui a cinco anos, \(nome) terá \(idade + 5) anos.")
```

O resultado será:

```
Daqui a cinco anos, João terá 35 anos.
```

## Aprofundamento:

A interpolação de strings não é uma inovação do Swift. Outras linguagens, como o Perl e o Ruby, também a implementam.

Há alternativas à interpolação de strings, como a concatenação de strings e o formato de string, mas a interpolação de strings oferece maior legibilidade e simplicidade. Por exemplo, em vez de `"Olá " + nome + "!"`, é mais simples escrever `"Olá, \(nome)!"`.

O Swift implementa a interpolação de strings convertendo tudo em uma representação de string. Mesmo números e respostas de funções podem ser convertidos e interpolados em uma string. 

## Veja Também:

- Documentação oficial da Apple sobre Strings e Caracteres em Swift: <a href='https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html'>https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html</a>
- Um ótimo guia para entender a interpolação de strings: <a href='https://www.hackingwithswift.com/articles/178/super-powered-string-interpolation-in-swift-5-0'>https://www.hackingwithswift.com/articles/178/super-powered-string-interpolation-in-swift-5-0</a> 
- Tutorial point em Swift: <a href='https://www.tutorialspoint.com/swift/swift_strings.htm'>https://www.tutorialspoint.com/swift/swift_strings.htm</a>