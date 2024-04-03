---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:28.086100-07:00
description: "Como fazer: As struct `String` de Swift v\xEAm com alguns m\xE9todos\
  \ embutidos para manipular a caixa de strings. Aqui est\xE3o algumas abordagens\
  \ para capitalizar\u2026"
lastmod: '2024-03-13T22:44:46.904643-06:00'
model: gpt-4-0125-preview
summary: "As struct `String` de Swift v\xEAm com alguns m\xE9todos embutidos para\
  \ manipular a caixa de strings."
title: Capitalizando uma string
weight: 2
---

## Como fazer:
As struct `String` de Swift vêm com alguns métodos embutidos para manipular a caixa de strings. Aqui estão algumas abordagens para capitalizar strings em Swift, incluindo o uso de métodos padrão e bibliotecas de terceiros, se necessário.

### Usando métodos embutidos
Para capitalizar a primeira letra de uma string e tornar o restante em minúsculas:

```swift
let myString = "hello, world"
let capitalizedString = myString.prefix(1).uppercased() + myString.dropFirst().lowercased()
print(capitalizedString) // Saída: "Hello, world"
```

Para capitalizar a primeira letra de cada palavra em uma sentença, você pode usar a propriedade `capitalized`:

```swift
let sentence = "hello, world"
let capitalizedSentence = sentence.capitalized
print(capitalizedSentence) // Saída: "Hello, World"
```

### Usando uma biblioteca de terceiros
Embora a biblioteca padrão do Swift seja bastante abrangente, alguns formatos de capitalização específicos podem exigir operações mais complexas ou podem ser simplificados usando bibliotecas de terceiros. Uma das populares para manipulação de strings é a SwiftRichString. (Nota: Sempre certifique-se de incluir bibliotecas de terceiros através do Swift Package Manager, CocoaPods ou Carthage, e importá-las no seu arquivo.)

Primeiro, você precisaria adicionar `SwiftRichString` ao seu projeto. Uma vez instalada, você pode usá-la para realizar várias operações com strings, incluindo necessidades específicas de capitalização. No entanto, até o momento, os métodos embutidos do Swift cobrem adequadamente a maioria dos casos de uso de capitalização sem a necessidade de bibliotecas externas apenas para capitalizar strings.

Sempre consulte a documentação mais recente da biblioteca para quaisquer atualizações ou mudanças nos métodos.
