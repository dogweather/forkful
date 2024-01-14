---
title:                "Swift: Usando expressões regulares"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por Que

Você já se deparou com a necessidade de pesquisar e manipular texto de forma precisa, mas não sabia como? As expressões regulares podem ser a solução para esse problema! Elas são padrões de texto que permitem buscar e manipular strings de uma forma poderosa e eficiente.

## Como Usar

Para utilizar expressões regulares em Swift, primeiro importe a biblioteca `Foundation` que contém a estrutura `NSRegularExpression` que nos permite criar e gerenciar expressões regulares. Em seguida, defina um padrão de texto utilizando a sintaxe específica das expressões regulares, por exemplo, para buscar um número de telefone no formato (XXX) XXX-XXXX podemos utilizar o seguinte padrão: `\\(\\d{3}\\)\\s\\d{3}-\\d{4}`. É importante notar que os símbolos `\` precisam ser duplicados para serem reconhecidos corretamente. Depois de definir o padrão, podemos utilizar o método `matches` da estrutura `NSRegularExpression` passando o texto e o padrão como parâmetros. Se houver correspondência, o resultado será um objeto do tipo `NSTextCheckingResult` que contém informações sobre a ocorrência encontrada.

```Swift
import Foundation
let texto = "(123) 456-7890"
let padrao = "\\(\\d{3}\\)\\s\\d{3}-\\d{4}"
let expressaoRegular = try! NSRegularExpression(pattern: padrao, options: [])
let resultado = expressaoRegular.matches(in: texto, options: [], range: NSRange(location: 0, length: texto.utf16.count))
```

Podemos então utilizar o método `range` do objeto `NSTextCheckingResult` para obter a posição e o tamanho da ocorrência encontrada no texto e utilizar o método `substring` para extrair a string correspondente.

```Swift
let posicao = resultado.first?.range
let telefone = texto.substring(with: posicao!)
print(telefone) // (123) 456-7890
```

## Mergulho Profundo

As expressões regulares suportam muitas outras funcionalidades além da busca de padrões simples. Alguns dos recursos mais avançados incluem o uso de grupos de captura para extrair informações específicas, a utilização de metacaracteres para representar conjuntos de caracteres, a utilização de quantificadores para indicar repetições de um padrão e a utilização de âncoras para indicar posições específicas no texto. Além disso, é possível utilizar modificadores para tornar as expressões case-insensitive ou definir um limite para as ocorrências buscadas. Para se aprofundar ainda mais, recomendamos a leitura da documentação oficial da Apple sobre expressões regulares em Swift.

## Veja Também

- [Documentação Oficial da Apple sobre Expressões Regulares](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Tutorial: Como usar expressões regulares em Swift](https://www.raywenderlich.com/6436037-regular-expressions-tutorial-for-swift-part-1)
- [Lista de metacaracteres e modificadores em Expressões Regulares](https://www.rexegg.com/regex-quickstart.html)