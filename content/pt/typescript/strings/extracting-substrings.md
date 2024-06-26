---
date: 2024-01-20 17:46:35.533615-07:00
description: "Como Fazer: Historicamente, a necessidade de trabalhar com partes de\
  \ strings sempre existiu. Em JavaScript, e consequentemente TypeScript, `substring`,\u2026"
lastmod: '2024-04-05T21:53:46.644968-06:00'
model: gpt-4-1106-preview
summary: Historicamente, a necessidade de trabalhar com partes de strings sempre existiu.
title: Extraindo substrings
weight: 6
---

## Como Fazer:
```TypeScript
let texto: string = "TypeScript é demais!";
let parte: string = texto.substring(0, 10); // "TypeScript"

console.log(parte); // Saída: TypeScript

// Uso de substr (atenção, é considerado legado!)
let outraParte: string = texto.substr(11, 7); // "é demais"

console.log(outraParte); // Saída: é demais

// Uso de slice para obter substrings também
let pedaco: string = texto.slice(11); // "é demais!"

console.log(pedaco); // Saída: é demais!
```

## Aprofundando
Historicamente, a necessidade de trabalhar com partes de strings sempre existiu. Em JavaScript, e consequentemente TypeScript, `substring`, `substr`, e `slice` são métodos tradicionalmente utilizados para essa finalidade. `substring` e `slice` são bem parecidos, mas `slice` pode aceitar índices negativos, o que torna possível começar do fim da string. Embora `substr` faça algo similar, é considerado um método legado e pode ser removido em futuras versões, então é melhor preferir `substring` ou `slice`. Em termos de implementação, esses métodos utilizam os conceitos de indexação de strings e iteram sobre os caracteres para criar a string resultante.

Alternativas modernas para lidar com strings incluem expressões regulares e métodos auxiliares de manipulação de strings, como `split` e `replace`, que podem também ser usados para extrair e manipular substrings de maneira mais flexível.

## Veja Também
- Documentação oficial do TypeScript sobre "String": https://www.typescriptlang.org/docs/handbook/basic-types.html#string
- MDN Web Docs sobre manipulação de strings em JavaScript (aplicável ao TypeScript): https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String
- Um curso online de TypeScript para aprofundar conhecimentos: https://www.typescriptlang.org/community/training.html
