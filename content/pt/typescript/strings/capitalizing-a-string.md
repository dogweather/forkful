---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:41.338634-07:00
description: "Capitalizar uma string envolve modificar o primeiro caractere de uma\
  \ string dada para mai\xFAsculo se ele estiver em min\xFAsculo, muitas vezes deixando\
  \ o resto\u2026"
lastmod: '2024-03-13T22:44:46.309073-06:00'
model: gpt-4-0125-preview
summary: "Capitalizar uma string envolve modificar o primeiro caractere de uma string\
  \ dada para mai\xFAsculo se ele estiver em min\xFAsculo, muitas vezes deixando o\
  \ resto da string inalterado."
title: Capitalizando uma string
weight: 2
---

## Como fazer:
TypeScript, sendo um superconjunto de JavaScript, permite várias maneiras de capitalizar strings, variando de abordagens puras de JavaScript a utilização de bibliotecas de terceiros para casos de uso mais complexos ou específicos.

**Abordagem Pura de JavaScript:**

```typescript
function capitalize(str: string): string {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

// Saída de Exemplo:
console.log(capitalize('hello TypeScript!')); // 'Hello TypeScript!'
```

Este método é direto e depende do método `charAt()` para acessar o primeiro caractere da string e `toUpperCase()` para convertê-lo para maiúsculo. O método `slice(1)` então recupera o resto da string, deixando-a inalterada.

**Usando a Biblioteca Lodash:**

Para projetos que já usam a biblioteca [Lodash](https://lodash.com/), você pode utilizar sua função `_.capitalize` para alcançar o mesmo resultado com menos código.

Primeiro, instale o Lodash:

```bash
npm install lodash
```

Então, use-a no seu arquivo TypeScript:

```typescript
import * as _ from 'lodash';

// Saída de Exemplo:
console.log(_.capitalize('hello TypeScript!')); // 'Hello typescript!'
```

Nota: O método `_.capitalize` do Lodash converte o resto da string para minúsculo, o que pode não ser sempre o que você deseja.

**Usando uma Expressão Regular:**

Uma expressão regular pode fornecer uma maneira concisa de capitalizar a primeira letra de uma string, especialmente se você precisar capitalizar a primeira letra de cada palavra em uma string.

```typescript
function capitalizeWords(str: string): string {
  return str.replace(/\b\w/g, char => char.toUpperCase());
}

// Saída de Exemplo:
console.log(capitalizeWords('hello typescript world!')); // 'Hello Typescript World!'
```

Este método usa a função `replace()` para procurar qualquer limite de palavra seguido por um caractere alfanumérico (`\b\w`), capitalizando cada correspondência. É particularmente útil para títulos ou cabeçalhos.
