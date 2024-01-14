---
title:    "TypeScript: Ecrire vers l'erreur standard"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous écrivez du code TypeScript, il est souvent nécessaire de faire face à des erreurs et des bogues. Ces erreurs peuvent être difficiles à trouver et à comprendre, mais heureusement, il existe une solution pour les afficher et les suivre plus facilement : écrire vers la sortie standard d'erreur, également appelée "standard error".

## Comment faire 

Pour écrire vers la sortie standard d'erreur en TypeScript, il suffit d'utiliser la méthode `console.error()` avec le message que vous souhaitez afficher. Par exemple :

```TypeScript
console.error("Une erreur s'est produite !");
```

Cela affichera le message "Une erreur s'est produite !" dans la console du navigateur ou du terminal.

## Plongée en profondeur 

Lorsque vous utilisez `console.error()` en TypeScript, il est important de noter que ce n'est pas seulement utile pour afficher des messages d'erreur. Vous pouvez également l'utiliser pour afficher des informations de débogage, telles que des valeurs de variables, pour mieux comprendre le fonctionnement de votre code. En outre, la méthode `console.error()` renvoie également un objet Error, qui peut être utile pour suivre les erreurs et les bogues dans votre code.

## Voir aussi

Pour en savoir plus sur l'écriture vers la sortie standard d'erreur en TypeScript, vous pouvez consulter les ressources suivantes :

- [Documentation officielle de TypeScript sur `console.error()`](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-3.html#error-log-tracing)
- [Guide pour le débogage en TypeScript](https://dev.to/klequis/debugging-typescript-an-introduction-and-guide-1n6e)
- [Vidéo tutoriel sur la sortie standard d'erreur en TypeScript](https://www.youtube.com/watch?v=xUl3HQrWec0)