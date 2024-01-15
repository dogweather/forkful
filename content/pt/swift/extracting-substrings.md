---
title:                "Extraindo subcadeias"
html_title:           "Swift: Extraindo subcadeias"
simple_title:         "Extraindo subcadeias"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que

Extração de substrings é uma técnica muito útil em programação Swift para obter partes específicas de uma string maior. Isso é especialmente útil para análise de texto e manipulação de dados.

## Como fazer

Para extrair uma substring em Swift, usamos o método `prefix()` ou `suffix()` dependendo do que queremos obter. Por exemplo, se quisermos extrair os primeiros 5 caracteres de uma string, podemos usar o seguinte código:

```Swift
let string = "Olá mundo!"
let substring = string.prefix(5)
print(substring) // saída: "Olá m"
```

Se quisermos extrair os últimos 5 caracteres, usamos o método `suffix()`:

```Swift
let string = "Olá mundo!"
let substring = string.suffix(5)
print(substring) // saída: "undo!"
```

Também podemos utilizar o método `dropFirst()` ou `dropLast()` para remover os primeiros ou últimos caracteres de uma string e retornar o restante como uma substring.

```Swift
let string = "Hello world!"
let substring = string.dropFirst(6)
print(substring) // saída: "world!"
```

Além disso, também podemos usar os métodos `lowercased()` e `uppercased()` para converter a substring para letras minúsculas ou maiúsculas, respectivamente.

## Deep Dive

Extrair substrings em Swift é uma operação eficiente, pois a linguagem tem um desempenho rápido e otimizado para trabalhar com strings. Além disso, as várias opções de métodos para extrair as substrings tornam possível trabalhar com diferentes cenários de programação.

Também é importante notar que ao utilizar os métodos `prefix()` ou `suffix()`, não estamos criando uma nova string, mas sim uma `Substring`. Isso significa que operações subsequentes nessa substring podem ser mais eficientes e não exigem a criação de uma nova string.

## Veja também

- [Documentação oficial da Apple sobre substrings em Swift](https://developer.apple.com/documentation/swift/substring)
- [Tutorial sobre como extrair substrings em Swift](https://www.hackingwithswift.com/articles/201/6/how-to-extract-a-substring-from-a-string-in-swift)
- [Guia completo de strings em Swift](https://www.swiftbysundell.com/basics/strings/)