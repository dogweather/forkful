---
title:                "Swift: Procurando e substituindo texto"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que é importante fazer uma busca e substituição de texto?

Fazer uma busca e substituição de texto é uma prática essencial para qualquer programador, pois permite realizar mudanças rápidas e eficientes em grandes quantidades de texto. Isso pode ser especialmente útil quando se trabalha com documentos ou códigos extensos, economizando muito tempo e esforço.

## Como fazer uma busca e substituição de texto em Swift?

Existem algumas maneiras diferentes de fazer uma busca e substituição de texto em Swift, mas a mais comum é usando o método `replacingOccurrences(of:with:)`. Veja abaixo um exemplo de como usar essa função:

```Swift
let string = "Este é um exemplo de texto com palavras repetidas."

let novaString = string.replacingOccurrences(of: "texto", with: "código")
print(novaString)

// Output: Este é um exemplo de código com palavras repetidas.
```

Neste exemplo, a função `replacingOccurrences` está substituindo a palavra "texto" por "código". Você também pode usar esta função para substituir caracteres específicos, como espaços em branco ou símbolos. Basta fornecer o caractere ou string que deseja substituir como o primeiro argumento e o caractere ou string substituto como o segundo argumento.

## Mais detalhes sobre busca e substituição de texto

Além do método `replacingOccurrences`, existem outras formas de realizar busca e substituição de texto em Swift, como o uso de expressões regulares. Expressões regulares são padrões usados para encontrar e substituir texto com base em determinados critérios. Por exemplo, você pode usar uma expressão regular para encontrar todas as ocorrências de uma determinada palavra, independentemente de como ela é escrita (maiúsculas, minúsculas, plural, singular, etc.).

Swift também possui outras funções e métodos que podem ser úteis para a busca e substituição de texto, como `replacingCharacters`, `replaceSubrange`, `replaceSubrange(of:)`, entre outros.

## Veja também

Aprenda mais sobre busca e substituição de texto em Swift com estes recursos adicionais:

- [Documentação oficial da Apple sobre o método replacingOccurrences](https://developer.apple.com/documentation/foundation/nsstring/1409226-replacingoccurrences)
- [Artigo sobre expressões regulares em Swift](https://www.raywenderlich.com/165660/swift-regular-expressions-tutorial)
- [Vídeo tutorial sobre busca e substituição de texto em Swift](https://www.youtube.com/watch?v=ZgfhnpagA80)