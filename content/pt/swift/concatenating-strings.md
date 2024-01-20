---
title:                "Concatenando strings"
html_title:           "Elixir: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

# Concatenando Strings em Swift: O Guia Abrangente

## O Que é & Porquê?

A concatenação de strings é o processo de juntar duas ou mais strings para formar uma única string. Os programadores o fazem para manipular e exibir dados de maneira mais eficiente e legível.

## Como Fazer:

Aqui está um exemplo simples de concatenação de strings em Swift.

```Swift
var str1 = "Olá, "
var str2 = "mundo!"
let str3 = str1 + str2
print(str3)  // Outputs "Olá, mundo!"
```
Também podemos concatenar strings usando o operador `+=`.

```Swift
var str1 = "Olá, "
let str2 = "mundo!"
str1 += str2
print(str1) // Outputs "Olá, mundo!"
```

## Mergulho Profundo

Historicamente, os programadores têm utilizado a concatenação de strings para tudo, desde a criação de mensagens complexas a construção de consultas SQL. Em Swift, o mecanismo de concatenação é otimizado para eficiência e simplicidade.

Existem outras maneiras de juntar strings em Swift, incluindo a utilização de templates de strings e a funcionalidade `join()` do Swift 4 que junta varias strings usando um separador.

Também é importante entender que a concatenação de strings em Swift é uma operação de custo linear — o tempo de execução aumenta proporcionalmente com o tamanho da string.

## Veja Também

Para mais detalhes e possibilidades ao concatenar strings, dê uma olhada nestes outros recursos:
- [Site oficial da Apple para Swift](https://developer.apple.com/swift/)
- [Guia oficial da linguagem Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Stack Overflow, uma comunidade de especialistas em codificação](https://stackoverflow.com/questions/tagged/swift)

Espero que este guia seja útil para seus projetos de programação em Swift. Pratique a concatenação de string e experimente por si mesmo!