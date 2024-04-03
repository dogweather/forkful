---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:11.228743-07:00
description: "Como Fazer: Criar e usar arrays associativos no TypeScript \xE9 simples.\
  \ Aqui est\xE1 um passo a passo b\xE1sico."
lastmod: '2024-03-13T22:44:46.318868-06:00'
model: gpt-4-0125-preview
summary: "Criar e usar arrays associativos no TypeScript \xE9 simples."
title: Usando arrays associativos
weight: 15
---

## Como Fazer:
Criar e usar arrays associativos no TypeScript é simples. Aqui está um passo a passo básico:

```TypeScript
// Declarando um array associativo
let user: { [key: string]: string } = {};

// Adicionando dados
user["name"] = "Jane Doe";
user["email"] = "jane@example.com";

console.log(user);
```

Saída:

```TypeScript
{ name: 'Jane Doe', email: 'jane@example.com' }
```

Iterar sobre pares de chave-valor é também fácil:

```TypeScript
for (let key in user) {
    console.log(key + ": " + user[key]);
}
```

Saída:

```TypeScript
name: Jane Doe
email: jane@example.com
```

E se você está lidando com uma mistura de tipos de dados, o sistema de tipos do TypeScript é útil:

```TypeScript
let mixedTypes: { [key: string]: string | number } = {};
mixedTypes["name"] = "John Doe";
mixedTypes["age"] = 30;

console.log(mixedTypes);
```

Saída:

```TypeScript
{ name: 'John Doe', age: 30 }
```

## Aprofundando
No TypeScript, o que nos referimos como arrays associativos são essencialmente objetos. Historicamente, em linguagens como PHP, arrays associativos são um tipo fundamental, mas JavaScript (e por extensão, TypeScript) usa objetos para esse propósito. Essa abordagem é tanto uma força quanto uma limitação. Objetos fornecem uma estrutura altamente dinâmica para associar strings a valores, mas eles não são destinados a ser usados como 'arrays' no sentido tradicional. Por exemplo, você não pode utilizar métodos de array como `push` ou `pop` diretamente nesses objetos.

Para casos onde você precisa de coleções ordenadas de pares de chave-valor com operações estilo array, TypeScript (e JavaScript moderno) oferece o objeto `Map`:

```TypeScript
let userMap = new Map<string, string>();
userMap.set("name", "Jane Doe");
userMap.set("email", "jane@example.com");

userMap.forEach((value, key) => {
    console.log(key + ": " + value);
});
```

Enquanto o sistema de tipos do TypeScript e recursos do ES6 como `Map` fornecem alternativas poderosas, entender como usar objetos como arrays associativos é útil para cenários onde literais de objeto são mais eficientes ou quando trabalhando com estruturas de dados JSON. É tudo sobre escolher a ferramenta certa para o trabalho.
