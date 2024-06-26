---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:17.962964-07:00
description: "Come fare: Creare e usare array associativi in TypeScript \xE8 semplice.\
  \ Ecco una guida di base."
lastmod: '2024-03-13T22:44:43.168971-06:00'
model: gpt-4-0125-preview
summary: "Creare e usare array associativi in TypeScript \xE8 semplice."
title: Utilizzo di array associativi
weight: 15
---

## Come fare:
Creare e usare array associativi in TypeScript è semplice. Ecco una guida di base:

```TypeScript
// Dichiarazione di un array associativo
let user: { [key: string]: string } = {};

// Aggiunta di dati
user["name"] = "Jane Doe";
user["email"] = "jane@example.com";

console.log(user);
```

Output:

```TypeScript
{ name: 'Jane Doe', email: 'jane@example.com' }
```

Iterare su coppie chiave-valore è altrettanto facile:

```TypeScript
for (let key in user) {
    console.log(key + ": " + user[key]);
}
```

Output:

```TypeScript
name: Jane Doe
email: jane@example.com
```

E se hai a che fare con un mix di tipi di dati, il sistema di tipi di TypeScript è utile:

```TypeScript
let mixedTypes: { [key: string]: string | number } = {};
mixedTypes["name"] = "John Doe";
mixedTypes["age"] = 30;

console.log(mixedTypes);
```

Output:

```TypeScript
{ name: 'John Doe', age: 30 }
```

## Approfondimento
In TypeScript, ciò che ci riferiamo come array associativi sono essenzialmente oggetti. Storicamente, in linguaggi come PHP, gli array associativi sono un tipo fondamentale, ma JavaScript (e per estensione TypeScript) usa gli oggetti per questo scopo. Questo approccio è sia un punto di forza che una limitazione. Gli oggetti forniscono una struttura altamente dinamica per associare stringhe a valori, ma non sono destinati a essere usati come 'array' nel senso tradizionale. Ad esempio, non puoi usare direttamente su questi oggetti metodi di array come `push` o `pop`.

Per casi in cui sono necessarie collezioni ordinate di coppie chiave-valore con operazioni simili agli array, TypeScript (e JavaScript moderno) offre l'oggetto `Map`:

```TypeScript
let userMap = new Map<string, string>();
userMap.set("name", "Jane Doe");
userMap.set("email", "jane@example.com");

userMap.forEach((value, key) => {
    console.log(key + ": " + value);
});
```

Mentre il sistema di tipi di TypeScript e le funzionalità di ES6 come `Map` offrono potenti alternative, capire come usare gli oggetti come array associativi è utile per scenari in cui i letterali degli oggetti sono più efficienti o quando si lavora con strutture di dati JSON. Si tratta di scegliere lo strumento giusto per il lavoro.
