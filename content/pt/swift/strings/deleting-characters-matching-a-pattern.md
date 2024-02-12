---
title:                "Excluindo caracteres que correspondem a um padrão"
aliases: - /pt/swift/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:43:12.882087-07:00
model:                 gpt-4-1106-preview
simple_title:         "Excluindo caracteres que correspondem a um padrão"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O Quê & Porquê?
Apagar caracteres que correspondem a um padrão é filtrar o que não queremos em uma string. Programadores fazem isso para limpar dados, validar entradas ou preparar textos para processamentos específicos.

## Como Fazer:
Para deletar caracteres indesejados em Swift, podemos usar `String` e `CharacterSet`. Veja como:

```Swift
var texto = "Olá, Mundo! 123."
let caracteresParaRemover = CharacterSet(charactersIn: "123.")
texto.unicodeScalars.removeAll(where: { caracteresParaRemover.contains($0) })
// Saída: "Olá, Mundo! "
```

Outro exemplo, removendo todas as vogais de uma frase:

```Swift
var frase = "Remover todas as vogais"
let vogais = CharacterSet(charactersIn: "aeiouAEIOU")
frase.unicodeScalars.removeAll(where: { vogais.contains($0) })
// Saída: "Rmvr tds s vgs"
```

## Mergulho Profundo
No passado, muitos usavam expressões regulares (regex) para essa tarefa – e ainda é uma opção viável. Em Swift, o `CharacterSet` e métodos de extensão de `String` facilitam o trabalho, principalmente para padrões simples. 

Para padrões mais complexos ou dinâmicos, regex ainda é o rei. Contudo, regex pode ser pesado e difícil de ler/manutenizar. O ideal é balancear simplicidade e desempenho, escolhendo a ferramenta certa para o problema certo.

Implementar o `removeAll(where:)` é eficiente porque itera pela coleção de `unicodeScalars` da string apenas uma vez, e Swift otimiza o uso de strings internamente com uma representação eficiente na maioria dos casos.

## Veja Também
- Documentação oficial do `CharacterSet` em Swift: [CharacterSet](https://developer.apple.com/documentation/foundation/characterset)
- Guia rápido para Expressões Regulares em Swift: [NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)
