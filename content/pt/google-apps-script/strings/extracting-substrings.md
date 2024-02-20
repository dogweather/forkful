---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:56.422363-07:00
description: "A extra\xE7\xE3o de substrings envolve pegar uma por\xE7\xE3o de uma\
  \ string - essencialmente, criar uma nova string a partir de uma parte de uma existente.\u2026"
lastmod: 2024-02-19 22:05:05.162933
model: gpt-4-0125-preview
summary: "A extra\xE7\xE3o de substrings envolve pegar uma por\xE7\xE3o de uma string\
  \ - essencialmente, criar uma nova string a partir de uma parte de uma existente.\u2026"
title: Extraindo substrings
---

{{< edit_this_page >}}

## O que & Por quê?

A extração de substrings envolve pegar uma porção de uma string - essencialmente, criar uma nova string a partir de uma parte de uma existente. Programadores fazem isso por uma miríade de motivos, incluindo análise de dados, manipulação de texto para interfaces de usuário, ou processamento de entrada para várias aplicações, tornando a extração de substring uma ferramenta versátil em qualquer arsenal de scripting.

## Como fazer:

No Google Apps Script, que é baseado no moderno JavaScript, a extração de substring pode ser alcançada através de vários métodos, incluindo `substring()`, `substr()` e `slice()`. Cada um tem suas nuances, mas todos servem ao propósito de extrair caracteres especificados de uma string.

```javascript
// Exemplo usando substring()
var str = "Hello, world!";
var result = str.substring(0, 5);
console.log(result); // Saída: Hello

// Exemplo usando substr()
var resultSubstr = str.substr(7, 5);
console.log(resultSubstr); // Saída: world

// Exemplo usando slice()
var resultSlice = str.slice(-6);
console.log(resultSlice); // Saída: world!
```

Cada método leva dois argumentos: a posição inicial e, exceto para `slice()` que pode aceitar índices negativos para começar do fim, a posição final ou o número de caracteres para extrair. Vale ressaltar que a string original permanece inalterada após essas operações, pois eles retornam novos valores de string.

## Mergulho Profundo

Historicamente, os métodos em JavaScript para extração de substrings têm sido fonte de confusão devido aos seus nomes e funcionalidades similares. Contudo, no Google Apps Script e no JavaScript moderno, `substring()` e `slice()` são mais frequentemente usados, com `substr()` sendo considerado obsoleto. Isso é importante de se notar para aqueles que escrevem código com visão de futuro.

A principal diferença entre `substring()` e `slice()` é como eles lidam com índices negativos; `substring()` trata índices negativos como 0, enquanto `slice()` pode aceitar um índice negativo para começar a extração do fim da string. Isso torna `slice()` particularmente útil para casos onde o comprimento exato da string pode não ser conhecido ou quando se precisa extrair do fim.

Ao decidir qual método usar para a extração de substring, a escolha muitas vezes se resume aos requisitos específicos da operação (por exemplo, se o tratamento de índices negativos é benéfico) e padrões pessoais ou de equipe de codificação. Embora não exista uma melhor prática que se aplique a todos os casos, entender as sutis diferenças e implicações de desempenho pode ajudar a tomar uma decisão informada.
