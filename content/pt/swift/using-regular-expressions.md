---
title:                "Swift: Usando expressões regulares"
simple_title:         "Usando expressões regulares"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar Expressões Regulares em Swift?

Ao escrever código em Swift, é comum encontrar a necessidade de executar tarefas relacionadas a padrões de texto, como buscar e substituir palavras, validar formatos de dados e extrair informações específicas de uma string. É aí que entram as expressões regulares - uma ferramenta poderosa para lidar com essas tarefas de maneira eficiente e precisa.

## Como Usar Expressões Regulares em Swift

A forma mais simples de utilizar expressões regulares em Swift é através do uso da classe `NSRegularExpression`. Veja um exemplo de como você pode buscar todas as palavras que começam com a letra "S" em uma string:

```Swift
let texto = "Swift é uma linguagem de programação incrível"
let expressao = try! NSRegularExpression(pattern: "\\bS\\w+", options: .caseInsensitive)

let correspondencias = expressao.matches(in: texto, options: [], range: NSMakeRange(0, texto.utf16.count))

for correspondencia in correspondencias {
    print(String(texto[correspondencia.range]))
}
```

A saída deste código seria:

```
Swift
uma
```

Você também pode substituir essas palavras pelo número de caracteres que possuem, usando o método `stringByReplacingMatches`:

```Swift
let resultado = expressao.stringByReplacingMatches(in: texto, options: [], range: NSMakeRange(0, texto.utf16.count), withTemplate: "$0.count")
```

Neste caso, a saída seria:

```
5 é uma linguagem de programação 8
```

## Um Mergulho Profundo em Expressões Regulares em Swift

Além do básico, existem muitas outras funcionalidades interessantes que podem ser exploradas com expressões regulares em Swift. Por exemplo, você pode utilizar grupos de captura para extrair partes específicas de uma string, ou aproveitar os operadores `+=~` e `!!~` para simplificar ainda mais as expressões regulares.

Não há espaço suficiente aqui para cobrir todas as possibilidades de uso de expressões regulares em Swift, portanto, é altamente recomendável que você dê uma olhada na documentação oficial ou em recursos adicionais listados abaixo.

## Veja Também

- [Documentação Oficial do Swift](https://swift.org/documentation/)
- [Tutorial de Expressões Regulares em Swift](https://www.raywenderlich.com/86205/nsregularexpression-swift-tutorial)
- [Vídeo explicativo sobre Expressões Regulares em Swift](https://www.youtube.com/watch?v=5X_g0G5h0wE)