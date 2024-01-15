---
title:                "Excluindo caracteres que correspondem a um padrão."
html_title:           "Swift: Excluindo caracteres que correspondem a um padrão."
simple_title:         "Excluindo caracteres que correspondem a um padrão."
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

O Swift é uma linguagem de programação moderna e poderosa que é usada para criar aplicativos para iOS, macOS, watchOS e tvOS. Como programador, é importante ter conhecimento sobre diferentes funcionalidades do Swift, incluindo a capacidade de excluir caracteres que correspondem a um determinado padrão. Isso pode economizar tempo e tornar o código mais eficiente.

## Como Fazer

Para excluir caracteres que correspondem a um padrão em Swift, você pode usar a função `replacingOccurrences(of:with:)`. Esta função substitui todas as ocorrências de uma string com uma nova string, e também tem um parâmetro opcional de `options` para especificar quaisquer opções adicionais.

Aqui está um exemplo de código que usa `replacingOccurrences(of:with:)` para excluir todos os caracteres de pontuação de uma string:

```
let frase = "Olá! Como vai?"
let semPontuacao = frase.replacingOccurrences(of: "[^A-Za-z0-9]", with: "", options: .regularExpression)
print(semPontuacao) // output: OláComovai
```

O parâmetro `options` neste exemplo especifica que apenas caracteres alfanuméricos devem ser mantidos na string. Isso significa que todos os outros caracteres, como o ponto e a exclamação, serão excluídos.

Você também pode optar por usar o `for-in loop` para percorrer a string e excluir manualmente as ocorrências de caracteres indesejados:

```
let frase = "Olá! Como vai?"
var semPontuacao = ""

for letra in frase {
  if !"[^A-Za-z0-9]".contains(String(letra)) { // verifica se a letra não é um caractere de pontuação
    semPontuacao += String(letra) // adiciona a letra à string semPontuacao
  }
}

print(semPontuacao) // output: OláComovai
```

Neste exemplo, usamos o método `contains()` para verificar se a `String` `"[^A-Za-z0-9]"` contém a letra atual na iteração. Se não contiver, isso significa que a letra não é um caractere de pontuação e é adicionada à string `semPontuacao`.

## Mergulho Profundo

Ao excluir caracteres que correspondem a um padrão em Swift, é importante entender como funciona o `options`, que é um parâmetro opcional de `replacingOccurrences(of:with:)`. Ele permite que você especifique opções adicionais, como ignorar letras maiúsculas e minúsculas, ou substituir apenas a primeira ocorrência de uma string.

Aqui estão algumas opções comuns que podem ser usadas:

- `.caseInsensitive`: ignora letras maiúsculas e minúsculas, o que significa que a string `Hello` seria considerada igual à string `hELLo`.
- `.diacriticInsensitive`: ignora marcas diacríticas, permitindo que letras acentuadas sejam consideradas iguais às letras não acentuadas.
- `.regularExpression`: permite que você use expressões regulares para especificar o padrão que deseja substituir.

Você também pode combinar várias opções, separando-as com o operador `|`. Por exemplo, se você quiser fazer uma substituição que ignore letras maiúsculas e minúsculas e marcas diacríticas, pode usar `.caseInsensitive | .diacriticInsensitive`.

## Veja Também

- [Documentação oficial do Swift](https://docs.swift.org/swift-book)
- [Curso de Swift do Codecademy](https://www.codecademy.com/learn/learn-swift)
- [Tutorial de Expressões Regulares em Swift](https://www.raywenderlich.com/162-regular-expressions-tutorial-getting-started)