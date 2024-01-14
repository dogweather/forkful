---
title:                "Swift: Convertendo uma string em letras minúsculas."
simple_title:         "Convertendo uma string em letras minúsculas."
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que

Quando estamos trabalhando com strings em Swift, muitas vezes precisamos convertê-las para letras minúsculas. Isso pode ser útil para garantir que nossa entrada de dados seja consistente, comparar strings de forma mais precisa ou simplesmente para deixar o texto mais legível.

## Como Fazer

A conversão de uma string para letras minúsculas em Swift é muito simples e pode ser feita de várias maneiras. Aqui estão alguns exemplos:

Para converter uma string para letras minúsculas usando o método `lowercased()`:

```Swift
let texto = "Olá, MUNDO!"
let textoMinúsculo = texto.lowercased()

print(textoMinúsculo)

// Saída: olá, mundo!
```

Para converter uma string para letras minúsculas usando as propriedades `capitalized` e `lowercase`:

```Swift
let texto = "ESTE TEXTO SERÁ CONVERTIDO"
let textoMinúsculo = texto.capitalized.lowercase

print(textoMinúsculo)

// Saída: este texto será convertido
```

Para converter uma string para letras minúsculas usando o método `map()`:

```Swift
let texto = "Eu Sou Uma String"
let textoMinúsculo = texto.map { String($0).lowercased() }.joined()

print(textoMinúsculo)

// Saída: eu sou uma string
```

## Dive Profundo

Ao converter uma string para letras minúsculas em Swift, é importante lembrar que o resultado será sempre uma nova string, pois as strings são imutáveis em Swift. Isso significa que, ao utilizar o método `lowercased()`, por exemplo, uma nova string será criada e retornada, enquanto a string original permanecerá inalterada.

Além disso, é importante mencionar que a conversão para letras minúsculas em Swift depende do idioma e localização do dispositivo do usuário. Isso significa que, para idiomas diferentes do inglês, os caracteres podem ser convertidos de forma diferente.

## Veja Também

- [Documentação oficial Apple sobre strings](https://developer.apple.com/documentation/swift/string)
- [Guia rápido de referência de strings em Swift](https://www.hackingwithswift.com/quick-start/swiftui/how-to-split-a-string-into-an-array)
- [Exemplos práticos de strings em Swift](https://www.raywenderlich.com/792-swift-4-strings-cheat-sheet)