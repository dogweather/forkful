---
title:    "Swift: Procurando e substituindo texto"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# Porquê

Às vezes, ao escrever código em Swift, pode ser necessário fazer alterações em várias partes do texto de uma vez. Pode ser algo simples, como corrigir um erro ortográfico, ou uma alteração mais complexa, como atualizar o nome de uma variável em todo o projeto. Ao invés de fazer essas alterações manualmente, podemos usar uma técnica chamada "search and replace" (buscar e substituir) para economizar tempo e esforço.

## Como Fazer

Em Swift, podemos usar o método `replacingOccurrences(of:with:)` para buscar e substituir texto em uma string. Ele recebe dois parâmetros: o texto que queremos substituir e o texto pelo qual queremos substituí-lo. Por exemplo, se tivermos uma string chamada `nome` com o valor "João", podemos usar o método para substituir todas as letras "o" por "a":

```swift
let nome = "João"
let novoNome = nome.replacingOccurrences(of: "o", with: "a")
// novoNome agora é "Jaão"
```

Outra opção é usar expressões regulares para buscar e substituir texto. Um exemplo é o método `replacingOccurrences(of:with:options:range:)`, que também recebe um parâmetro `options` para especificar as regras da expressão regular e um parâmetro `range` para limitar a busca a uma parte específica da string.

```swift
let texto = "Hoje é dia 31/12/2020"
let novoTexto = texto.replacingOccurrences(of: "\\d{2}\\/\\d{2}\\/\\d{4}", with: "data", options: .regularExpression)
// novoTexto agora é "Hoje é dia data"
```

## Mergulho Profundo

Além dos métodos mencionados acima, também podemos usar o framework Foundation para um controle mais avançado sobre buscar e substituir texto em Swift. Por exemplo, podemos usar a classe `NSRegularExpression` para criar e manipular expressões regulares, ou a classe `NSString` para fazer a busca e substituição em strings específicas.

Um cuidado que devemos ter é com o tipo de caracteres que usamos. Em Swift, os caracteres acentuados como "á" e "é" são tratados como unicode, portanto podem gerar resultados inesperados em algumas operações de busca e substituição. Para lidar com isso, podemos usar a função `localizedStandardContains` da classe `NSString`, que irá tratar esses caracteres de forma consistente em diferentes idiomas.

# Veja Também

- [Documentação oficial da Apple sobre NSString](https://developer.apple.com/documentation/foundation/nsstring)
- [Guia completo para expressões regulares em Swift](https://www.swiftbysundell.com/basics/regular-expressions/)
- [Tutorial em português sobre busca e substituição em strings em Swift](https://medium.com/@andrebralins/busca-e-substitui%C3%A7%C3%A3o-de-texto-com-swift-e2ea8af92f74)