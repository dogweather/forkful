---
title:                "Swift: Procurando e substituindo texto"
simple_title:         "Procurando e substituindo texto"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Porque

Ao programar em Swift, ocasionalmente nos deparamos com a necessidade de fazer alterações em partes específicas de um texto. Para evitar a tediosa tarefa de encontrar e substituir manualmente, podemos usar o recurso de busca e substituição presente na linguagem.

## Como fazer

Podemos realizar a busca e substituição de texto em Swift usando o método `replacingOccurrences(of:with:)`. Ele recebe dois parâmetros: o texto que queremos substituir e o texto pelo qual queremos substituir. Por exemplo:

```Swift
let texto = "Olá meu nome é Maria"
let novoTexto = texto.replacingOccurrences(of: "Maria", with: "João")
print(novoTexto) // Saída: "Olá meu nome é João" 
```

Além disso, podemos usar a opção `options` para especificar se queremos fazer a substituição apenas da primeira ocorrência ou de todas as ocorrências. O valor padrão é `.literal`, o que significa que o texto será substituído exatamente como foi fornecido.

```Swift
let texto = "apple apple apple orange"
let novoTexto = texto.replacingOccurrences(of: "apple", with: "banana", options: .caseInsensitive)
print(novoTexto) // Saída: "banana banana banana orange" 
```

Caso queira substituir apenas a primeira ocorrência, podemos usar `.caseInsensitive`, que ignora maiúsculas e minúsculas, tornando a substituição de texto mais flexível e abrangente.

## Deep Dive

Por baixo dos panos, o método `replacingOccurrences(of:with:)` usa expressões regulares para encontrar e substituir o texto. Com isso, podemos usar padrões mais avançados para encontrar e substituir texto com mais precisão. 

Por exemplo, podemos querer substituir todas as ocorrências de números por "#" em um texto. Podemos fazer isso usando o seguinte código:

```Swift
let texto = "A Mars órbita o sol em 687 dias."
let novoTexto = texto.replacingOccurrences(of: "[0-9]+", with: "#", options: .regularExpression)
print(novoTexto) // Saída: "A Mars órbita o sol em # dias."
```

Nesse exemplo, usamos `"[0-9]+"`, que significa que estamos procurando por dígitos numéricos de 0 a 9 e o sinal de "+" indica que queremos substituir todas as ocorrências.

## Veja também

- [Documentação oficial do método `replacingOccurrences(of:with:)`](https://developer.apple.com/documentation/foundation/nsstring/1413816-replacingoccurrences)
- [Expressões regulares em Swift](https://www.swiftbysundell.com/articles/regular-expressions-in-swift/)