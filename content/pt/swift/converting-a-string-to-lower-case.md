---
title:    "Swift: Convertendo uma string para minúsculas"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Por que

A conversão de uma string para letras minúsculas é uma tarefa comum na programação Swift. Isso pode ser útil para comparar strings sem diferenciar entre maiúsculas e minúsculas ou para padronizar a aparência visual de uma string.

## Como Fazer

Abaixo estão alguns exemplos de como converter uma string para minúsculas em Swift:

```Swift
let minhaString = "Bem-vindo ao meu blog!"
print(minhaString.lowercased())
// Output: bem-vindo ao meu blog!

let outraString = "PROGRAMANDO Em SWIFT é Divertido"
print(outraString.lowercased())
// Output: programando em swift é divertido
```

Nesses exemplos, usamos o método `lowercased()` em uma string para convertê-la para letras minúsculas. Esse método retorna uma nova string com todas as letras alteradas para minúsculas.

Também é possível usar o operador de atribuição `=` para atualizar diretamente a string original, como mostrado abaixo:

```Swift
var texto = "Eu GOSTO de Programar!"
texto = texto.lowercased()
print(texto)
// Output: eu gosto de programar!
```

## Mergulho Profundo

Por baixo dos panos, o método `lowercased()` usa as regras de localização da linguagem atual do dispositivo para converter as letras da string para a versão em minúsculas. Isso significa que, dependendo do idioma, os caracteres convertidos podem ser diferentes. Por exemplo, o caractere "I" em inglês é convertido para "i", enquanto em turco é convertido para "ı". Isso é importante a ser considerado ao lidar com strings em diferentes idiomas.

Além disso, é possível personalizar o comportamento da conversão de maiúsculas e minúsculas, fornecendo um `Locale` como parâmetro opcional para o método `lowercased()`.

## Veja Também

- [Documentação oficial da Apple sobre o método `lowercased()`](https://developer.apple.com/documentation/swift/string/3126845-lowercased)
- [Guia completo de localização em Swift](https://www.hackingwithswift.com/articles/158/how-to-localize-your-apps-in-swift)
- [Tutorial sobre conversão de strings em Swift](https://www.appcoda.com/string-conversion-swift/)