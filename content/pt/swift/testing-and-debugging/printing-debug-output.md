---
title:                "Exibindo saídas de depuração"
aliases:
- /pt/swift/printing-debug-output.md
date:                  2024-01-20T17:53:36.671081-07:00
model:                 gpt-4-1106-preview
simple_title:         "Exibindo saídas de depuração"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (O Quê & Por Quê?)
Vamos falar sobre impressão de saídas de depuração: é colocar na tela informações do que está rolando no seu código. Fazemos isso para saber o que está acontecendo lá dentro durante a execução, achando erros ou otimizando processos.

## How To (Como Fazer)
Simples e direto: use `print()` para jogar na tela o que você precisa. Se liga nos exemplos:

```Swift
// Imprimir uma mensagem simples
print("Oi, estou aqui!")

// Combinar texto e variáveis
let fruta = "banana"
print("Eu gosto de comer \(fruta)s.")

// Versão mais sofisticada, printando várias variáveis
let preco = 3.5
let quantidade = 2
print("Preço total por \(quantidade) \(fruta)s: \(preco * Double(quantidade))")
```

Saída:
```
Oi, estou aqui!
Eu gosto de comer bananas.
Preço total por 2 bananas: 7.0
```

## Deep Dive (Mergulho Profundo)
Antigamente, a gente usava o `println` para isso no Swift, mas ficou old school e foi removido na versão 2.0. Outra opção é usar `debugPrint()` quando precisar de uma saída mais detalhada para depuração. Ele é legal porque mostra mais infos, como a estrutura interna do que você está printando.

Vamos a um exemplo do `debugPrint()`:

```Swift
struct Pessoa {
    var nome: String
    var idade: Int
}

let fulano = Pessoa(nome: "João", idade: 28)
debugPrint(fulano)
```

Saída:
```
Pessoa(nome: "João", idade: 28)
```

Saca só: com `debugPrint()`, você tem uma visualização formatada da instância do struct, o que facilita a vida na hora de depurar.

## See Also (Veja Também)
- [Documentação oficial do Swift sobre Printing](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID309)
- [Artigo sobre debugging em Swift da Ray Wenderlich](https://www.raywenderlich.com/2107-debugging-with-xcode)
- [WWDC video sobre debugging em Swift](https://developer.apple.com/videos/play/wwdc2018/412/)
