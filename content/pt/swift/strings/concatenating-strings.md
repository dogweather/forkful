---
title:                "Concatenando strings"
aliases: - /pt/swift/concatenating-strings.md
date:                  2024-01-20T17:35:53.597743-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenando strings"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Concatenar strings significa juntar duas ou mais sequências de caracteres para formar uma nova string. Programadores fazem isso para construir mensagens, formatar texto dinamicamente ou simplesmente juntar informações.

## Como Fazer:

Swift torna a concatenação de strings direta e descomplicada. Vamos ver alguns exemplos:

```Swift
// Concatenação simples com o operador '+'
let saudacao = "Olá, "
let nome = "João!"
let mensagem = saudacao + nome
print(mensagem) // "Olá, João!"

// Usando interpolação de strings
let planeta = "mundo"
let mensagemInterpolada = "\(saudacao) \(planeta)!"
print(mensagemInterpolada) // "Olá, mundo!"

// Juntando várias strings com o método 'append'
var listaDeCompras = "Leite"
listaDeCompras.append(", Pão")
listaDeCompras.append(", Ovos")
print(listaDeCompras) // "Leite, Pão, Ovos"

// Concatenação com o operador '+='
var frase = "Programar é"
frase += " divertido!"
print(frase) // "Programar é divertido!"
```

## Aprofundando:

A concatenação de strings existe desde os primórdios das linguagens de programação, e seus mecanismos variam de uma para outra. Em Swift, essa operação é otimizada para ser eficiente e consumir pouca memória possível. Por exemplo, quando você usa o operador `+`, o Swift junta as strings de maneira inteligente, evitando a criação de várias cópias intermediárias.

Alternativas à concatenação direta incluem o uso de `Array` e a função `join`, que pode ser mais eficiente para uma grande quantidade de strings. Além disso, a escolha entre o operador `+`, o método `append` ou interpolação pode ser uma questão de legibilidade e preferência pessoal.

Detalhes de implementação como a cópia de strings na memória são abstraídos em Swift. Quando você manipula strings, a linguagem usa uma representação otimizada internamente para garantir que as operações de concatenação sejam rápidas, mesmo para strings grandes.

## Veja Também:

Para aprofundar seu conhecimento sobre strings em Swift e explorar tópicos correlatos, confira:

- [A Swift Tour - Strings and Characters](https://docs.swift.org/swift-book/GuidedTour/GuidedTour.html#ID297) from the official Swift documentation.
- [String](https://developer.apple.com/documentation/swift/string) documentation from Apple Developer.
- [Swift Programming: Dealing with Strings](https://www.raywenderlich.com/7738344-swift-formatting) for more examples and best practices.
