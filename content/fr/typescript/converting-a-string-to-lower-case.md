---
title:    "TypeScript: Convertir une chaîne en minuscules"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Convertissez-vous souvent des chaînes de caractères en minuscules? Peut-être pour normaliser les entrées utilisateur ou pour effectuer des recherches plus efficaces dans une base de données. Quelle que soit la raison, il est important de comprendre comment le faire correctement en TypeScript.

## Comment faire

Pour convertir une chaîne de caractères en minuscules en TypeScript, nous pouvons utiliser la méthode `toLowerCase()`. Voici un exemple de code qui prend une chaîne de caractères en entrée et renvoie la même chaîne en minuscules :

```typescript
// Définir une chaîne de caractères en entrée
let chaine = "BONJOUR";

// Convertir la chaîne en minuscules
let chaineEnMinuscules = chaine.toLowerCase();

// Afficher la nouvelle chaîne en minuscules
console.log(chaineEnMinuscules); // affiche "bonjour"
```

Comme vous pouvez le voir, la méthode `toLowerCase()` a été appelée sur la variable contenant la chaîne de caractères et le résultat a été stocké dans une nouvelle variable. Vous pouvez également utiliser cette méthode directement sur une chaîne de caractères littérale :

```typescript
let chaine = "BONJOUR".toLowerCase(); // chaine vaut maintenant "bonjour"
```

Cela peut être utile lorsque vous avez besoin de manipuler des chaînes de caractères en minuscules dans une seule ligne de code.

## Une plongée plus profonde

Il est important de noter que la méthode `toLowerCase()` ne modifie pas la chaîne originale, elle renvoie plutôt une nouvelle chaîne de caractères en minuscules. Cela peut être utile si vous avez besoin de garder la chaîne originale intacte. De plus, cette méthode utilise la norme Unicode pour effectuer la conversion en minuscules, ce qui peut entraîner des différences avec d'autres langages de programmation qui utilisent d'autres normes.

De plus, si votre chaîne de caractères contient des caractères accentués, la méthode `toLowerCase()` les convertira également en minuscules. Par exemple, la chaîne "ÉCOLE" sera convertie en "école". Si vous ne souhaitez pas que cela se produise, vous pouvez utiliser la méthode `toLocaleLowerCase()` qui prend en compte la langue locale pour effectuer la conversion.

## Voir aussi

- [Documentation officielle TypeScript - Méthode toLowerCase](https://www.typescriptlang.org/docs/handbook/strings.html#lowercase-and-uppercase)
- [Documentation officielle TypeScript - Méthode toLocaleLowerCase](https://www.typescriptlang.org/docs/handbook/strings.html#internationalization)
- [Unicode.org - Norme Unicode](https://unicode.org/standard/versions/fr-UL/july13.html)