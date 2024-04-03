---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:48:48.795489-07:00
description: "Como fazer: Google Apps Script, sendo baseado em JavaScript, permite\
  \ v\xE1rios m\xE9todos para capitalizar uma string, embora sem uma fun\xE7\xE3o\
  \ embutida. Aqui\u2026"
lastmod: '2024-03-13T22:44:46.088327-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script, sendo baseado em JavaScript, permite v\xE1rios m\xE9\
  todos para capitalizar uma string, embora sem uma fun\xE7\xE3o embutida."
title: Capitalizando uma String
weight: 2
---

## Como fazer:
Google Apps Script, sendo baseado em JavaScript, permite vários métodos para capitalizar uma string, embora sem uma função embutida. Aqui estão alguns exemplos sucintos:

**Método 1: Usando charAt() e slice()**

```javascript
function capitalizeString(inputString) {
  if (!inputString) return '';
  return inputString.charAt(0).toUpperCase() + inputString.slice(1).toLowerCase();
}

// Exemplo de uso
let result = capitalizeString('olá, mundo');
console.log(result);  // Saída: Olá, mundo
```

**Método 2: Usando uma Regex**

Para aqueles que preferem uma solução baseada em regex para lidar com casos extremos de maneira mais elegante:

```javascript
function capitalizeStringRegex(inputString) {
  return inputString.toLowerCase().replace(/^\w/, c => c.toUpperCase());
}

// Exemplo de uso
let result = capitalizeStringRegex('olá, mundo');
console.log(result);  // Saída: Olá, mundo
```

Ambos os métodos garantem que o primeiro caractere da string seja maiúsculo e os restantes sejam minúsculos, adequados para uma variedade de aplicações incluindo, mas não limitado a, manipulação de Google Sheets ou edição de documentos via Apps Script.

## Aprofundamento
Capitalizar strings no Google Apps Script é simples, aproveitando as poderosas capacidades de manipulação de string do JavaScript. Historicamente, linguagens como Python oferecem métodos embutidos como `.capitalize()` para alcançar isso, colocando um passo extra para programadores de JavaScript e Apps Script. No entanto, a ausência de uma função embutida em JavaScript/Google Apps Script incentiva a flexibilidade e um entendimento mais profundo das técnicas de manipulação de string.

Para cenários complexos, como capitalizar cada palavra em uma string (Caso de Título), os programadores podem combinar métodos regex com funções `split()` e `map()` para processar cada palavra individualmente. Embora o Google Apps Script não forneça um método direto para capitalização de string, o uso dos métodos de manipulação de string do JavaScript existentes oferece ampla flexibilidade, permitindo que os desenvolvedores manipulem strings de forma eficiente de acordo com suas necessidades específicas.

Em casos onde o desempenho e a eficiência são primordiais, vale ressaltar que a manipulação direta de strings pode ser mais performática do que regex, especialmente para strings mais longas ou operações dentro de grandes laços. No entanto, para a maioria das aplicações práticas dentro do Google Apps Script, ambos os métodos fornecem soluções confiáveis.
