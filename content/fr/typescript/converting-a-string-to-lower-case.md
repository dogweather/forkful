---
title:                "Convertir une chaîne en minuscules"
html_title:           "PHP: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi? 

La conversion d'une chaîne en minuscules est l'action de transformer tous les caractères alphabétiques en minuscules. Les programmeurs font cela pour une comparaison insensible à la casse, car `ABCDE` est différent de `abcde` dans la plupart des langages de programmation, dont TypeScript.

## Comment faire:

Voici comment convertir une chaîne en minuscules en TypeScript :

```TypeScript
let chaine = 'Ceci Est Une ChaîNe';
let minuscule = chaine.toLowerCase();

console.log(minuscule); // affiche 'ceci est une chaîne'
```

Tous les caractères dans la chaîne transformée sont en minuscules.

## Plongée en profondeur

Nous utilisons la transformation des chaînes en minuscules depuis les premiers jours du codage pour plusieurs raisons. Mellier que `toLowerCase()`, vous pouvez utiliser `toLocaleLowerCase()` pour respecter les paramètres régionaux de l'utilisateur. 

```TypeScript
let texte = 'ÇA VA';
console.log(texte.toLocaleLowerCase('fr-FR')); // affiche 'ça va'
```

C'est utile si vous travaillez avec des langues régionales, car chaque langue a ses propres règles de changement de casse.

## Voir aussi

- Documentation officielle TypeScript: https://www.typescriptlang.org/docs/
- Conversion de chaînes en TypeScript: https://www.typescripttutorial.net/typescript-tutorial/typescript-string-tolowercase/
- Comparaison de chaînes en JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase

C'est tout! Une utilisation basique et efficace de la conversion d'une chaîne en minuscules en TypeScript. Musclez votre code!