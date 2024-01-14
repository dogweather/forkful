---
title:    "Swift: Utilizando expressões regulares"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que utilizar Expressões Regulares em Swift

As expressões regulares são uma ferramenta essencial para trabalhar com strings em Swift. Elas permitem que você encontre padrões específicos dentro de uma string, tornando mais fácil a manipulação e análise de dados. Além disso, aprender a usar expressões regulares pode economizar muito tempo em tarefas de programação repetitivas.

## Como utilizar Expressões Regulares em Swift

Para utilizar expressões regulares em Swift, primeiro você precisa importar o framework `Foundation`:

```Swift
import Foundation
```

Em seguida, você pode criar um objeto `NSRegularExpression` passando a expressão regular desejada e as opções de busca como parâmetros:

```Swift
let regex = try NSRegularExpression(pattern: "[0-9]+", options: .anchorsMatchLines)
```

Você também pode usar o operador de atribuição `?=~` para fazer uma correspondência de expressão regular diretamente em uma string:

```Swift
let string = "Tenho 28 anos"
if string ~= "[0-9]+ anos" {
    print("Encontrou uma idade na string")
}
```

O resultado seria "Encontrou uma idade na string".

## Aprofundando-se no uso de Expressões Regulares

Existem muitas opções e métodos disponíveis ao trabalhar com expressões regulares em Swift. Você pode usar grupos de captura para extrair partes específicas de uma string, usar metacaracteres para tornar suas expressões regulares mais poderosas e aplicar modificadores de caso como `(?i)` para fazer buscas insensíveis a maiúsculas e minúsculas.

Também é importante lembrar que as expressões regulares seguem uma sintaxe específica e aprender esses padrões pode ser desafiador no começo. É altamente recomendado fazer alguns tutoriais e praticar bastante para se familiarizar com o uso de expressões regulares em Swift.

## Veja também

Aqui estão alguns links úteis para aprender mais sobre expressões regulares em Swift:

- Documentação oficial da Apple sobre Expressões Regulares em Swift: https://developer.apple.com/documentation/foundation/nsregularexpression
- Tutorial em vídeo sobre Expressões Regulares em Swift: https://www.youtube.com/watch?v=lS7alBPbnSU
- Lista de alguns metacaracteres úteis para expressões regulares em Swift: https://developer.apple.com/library/archive/documentation/NetworkingInternet/Conceptual/NetworkingTopics/Articles/UsingSocketsandSocketStreams.html#//apple_ref/doc/uid/TP40012533-SW7

Com esses recursos, você estará no caminho certo para se tornar um especialista em expressões regulares em Swift. Boa sorte!