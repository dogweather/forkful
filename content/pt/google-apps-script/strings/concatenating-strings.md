---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:50:08.108299-07:00
description: "Concatenar strings envolve combinar duas ou mais strings em uma \xFA\
  nica string. Programadores fazem isso para construir dinamicamente mensagens, URLs,\
  \ ou\u2026"
lastmod: '2024-03-13T22:44:46.098398-06:00'
model: gpt-4-0125-preview
summary: "Concatenar strings envolve combinar duas ou mais strings em uma \xFAnica\
  \ string."
title: Concatenando strings
weight: 3
---

## Como fazer:
No Google Apps Script, que é baseado em JavaScript, existem várias maneiras de concatenar strings. Aqui estão alguns métodos comuns:

### Usando o operador de soma (`+`):
```javascript
var firstName = "John";
var lastName = "Doe";
var fullName = firstName + " " + lastName;
Logger.log(fullName); // Saída: John Doe
```

### Usando o método `concat()`:
```javascript
var string1 = "Olá";
var string2 = "Mundo";
var combinedString = string1.concat(" ", string2);
Logger.log(combinedString); // Saída: Olá Mundo
```

### Usando literais de template (crases):
Esta é uma maneira moderna e flexível de concatenar strings, permitindo que você incorpore facilmente expressões dentro das strings.

```javascript
var language = "Google Apps Script";
var message = `Aprender ${language} é divertido!`;
Logger.log(message); // Saída: Aprender Google Apps Script é divertido!
```

Cada um desses métodos tem seus casos de uso, e a escolha entre eles geralmente depende de requisitos de legibilidade e da complexidade das strings sendo concatenadas.

## Aprofundamento
A concatenação de strings é um aspecto fundamental não apenas do Google Apps Script, mas de muitas linguagens de programação. Historicamente, a concatenação de strings era frequentemente realizada usando o operador de soma ou funções/métodos especializados como `concat()`. No entanto, com a introdução dos literais de template no ECMAScript 2015 (ES6), que o Google Apps Script suporta, os desenvolvedores ganharam uma maneira mais poderosa e intuitiva de lidar com strings.

Os literais de template não apenas simplificam a sintaxe para incorporar expressões dentro de strings, mas também suportam strings multilinha sem a necessidade de caracteres de nova linha explícitos. Isso reduz o potencial para erros e melhora a legibilidade do código, especialmente ao lidar com strings complexas ou ao substituir múltiplas variáveis em um template de texto.

Enquanto o operador `+` e o método `concat()` ainda são amplamente usados e suportados para compatibilidade com versões anteriores e simplicidade em cenários mais simples, os literais de template oferecem uma alternativa moderna e expressiva que é frequentemente considerada superior para concatenação de strings, especialmente quando a legibilidade e a manutenibilidade são uma preocupação.

No entanto, é importante escolher o método que melhor se adequa ao contexto e requisitos específicos do seu projeto, considerando fatores como a compatibilidade do ambiente alvo (embora isso raramente seja um problema com o Google Apps Script), implicações de desempenho (mínimas para a maioria das aplicações) e a familiaridade da equipe de desenvolvimento com recursos modernos do JavaScript.
