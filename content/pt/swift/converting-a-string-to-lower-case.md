---
title:                "Convertendo uma string para caixa baixa"
html_title:           "Swift: Convertendo uma string para caixa baixa"
simple_title:         "Convertendo uma string para caixa baixa"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que
 Converter uma string para minúsculo é uma tarefa comum em programação e pode ser útil ao lidar com entradas de usuário, realizar comparações de strings e outras operações de processamento de texto.

## Como Fazer
O processo de conversão de uma string para minúsculo é bastante simples em Swift. Basta usar o método `lowercased()` em uma string existente para transformá-la em letras minúsculas. Por exemplo:

```Swift
var minhaString = "Olá MUNDO!"
print(minhaString.lowercased())
```
Isso imprimirá "olá mundo!" na saída do console.

Outro método útil é o `localizedLowercase`, que permite converter uma string para minúsculo no idioma preferido do dispositivo do usuário. Por exemplo:

```Swift
var minhaString = "HELLO WORLD!"
print(minhaString.localizedLowercase)
```

## Deep Dive
Ao converter uma string para minúsculo, é importante ser cuidadoso com as regras de capitalização específicas do idioma. Por exemplo, em turco, a letra "i" maiúscula se torna "İ" minúscula, em vez de "i". Isso pode afetar a precisão dos resultados ao realizar comparações de strings. Portanto, é importante conhecer as regras de capitalização do idioma das strings com que estamos lidando.

Uma maneira de contornar esses problemas é usar o método `lowercased(with:)`, que leva em consideração as regras de capitalização do idioma especificado. Por exemplo:

```Swift
var minhaString = "eĺ pelosotros👫"
print(minhaString.lowercased(with: .latin))
```

Este código produzirá "el pelosotros👫" como saída, mesmo se tentarmos convertê-lo para minúsculo usando o método `lowercased()`.

## Veja Também
* [Documentação Oficial - String](https://developer.apple.com/documentation/swift/string)
* [Tutorial de Strings em Swift](https://www.hackingwithswift.com/quick-start/understanding-swift/whats-the-difference-between-a-string-and-a-string)
* [Capitalize a String in Swift](https://medium.com/better-programming/string-manipulation-in-swift-2c21a0378b0a)