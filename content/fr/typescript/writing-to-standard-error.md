---
title:                "Écrire dans l'erreur standard"
date:                  2024-01-19
simple_title:         "Écrire dans l'erreur standard"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Écrire sur la sortie d'erreur standard (stderr) permet de séparer les messages d'erreur des autres sorties de votre programme. Les programmeurs font ça pour faciliter le débogage et la gestion d'erreurs.

## How to:
Pour écrire sur stderr en TypeScript, utilisez `process.stderr`.

```TypeScript
process.stderr.write('Ceci est une erreur\n');
```

Sortie:

```
Ceci est une erreur
```

Vous pouvez aussi utiliser `console.error`:

```TypeScript
console.error('Oups, une autre erreur');
```

Sortie:

```
Oups, une autre erreur
```

## Deep Dive
Avant, en C, stderr était typiquement utilisé pour séparer les entrées/sorties standards (stdin/stdout) des erreurs. Comme en C, en TypeScript, stderr est non-bufferisé par défaut, donc les messages d'erreur s'affichent immédiatement. Alternativement, on pourrait écrire dans un fichier de log, mais stderr permet de voir les messages d'erreur directement dans la console. L'utilisation de stderr est standard pour les outils en ligne de commande et contribue à une meilleure gestion de flux.

## See Also
- Node.js documentation on process.stderr: [Node.js process.stderr](https://nodejs.org/api/process.html#processstderr)
- Console API reference: [MDN Web Docs - Console](https://developer.mozilla.org/en-US/docs/Web/API/Console)
- Unix Standard Streams: [Wikipedia - Standard streams](https://en.wikipedia.org/wiki/Standard_streams)
