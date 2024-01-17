---
title:                "Unindo strings"
html_title:           "Swift: Unindo strings"
simple_title:         "Unindo strings"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

# O que & Por quê?
String concatenation é uma técnica comum em programação onde duas ou mais strings são combinadas para formar uma nova string. Programadores geralmente usam concatenação de strings para criar mensagens personalizadas ou para combinar variáveis e textos em uma única linha de texto.

# Como fazer:
```Swift
let nome = "Maria"
let sobrenome = "Silva"
let mensagem = "Olá \(nome) \(sobrenome), seja bem-vinda!"
print(mensagem)
// Output: Olá Maria Silva, seja bem-vinda!
```

```Swift
let idade = 30
let texto = "Eu tenho \(idade) anos"
print(texto)
// Output: Eu tenho 30 anos
```

# Mergulho profundo:
Concatenação de strings não é uma técnica nova, sendo utilizada desde os primeiros dias da computação. Antes do surgimento das linguagens de programação, programadores de baixo nível precisavam combinar strings manualmente, o que era uma tarefa tediosa e propensa a erros. Hoje em dia, existem alternativas mais eficientes, como o uso de templates de string ou interpolação de string, mas a concatenação de strings ainda é amplamente utilizada.

# Veja também:
- [Documentação oficial do Swift sobre String Concatenation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Artigo da Apple sobre Interpolation de strings](https://developer.apple.com/library/archive/documentation/Swift/Conceptual/Swift_Programming_Language/StringsAndCharacters.html#//apple_ref/doc/uid/TP40014097-CH7-ID292)
- [Vídeo tutorial sobre Templates de String no Swift](https://www.youtube.com/watch?v=ZdZLlJxWa6A)