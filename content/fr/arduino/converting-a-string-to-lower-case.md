---
title:    "Arduino: Convertir une chaîne de caractères en minuscules."
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Pourquoi

Il est souvent nécessaire de convertir une chaîne de caractères en minuscules lors de la programmation Arduino. Cela permet de faciliter la comparaison de chaînes de caractères et de rendre les opérations de traitement de données plus cohérentes.

## Comment faire

Pour convertir une chaîne de caractères en minuscules dans un programme Arduino, vous pouvez utiliser la fonction ```Arduino toLowerCase()```. Voici un exemple de code montrant comment l'utiliser :

```
String str = "Bonjour Arduino!";
str.toLowerCase(); // convertit la chaîne en "bonjour arduino!"

```

Lorsque vous utilisez cette fonction, assurez-vous de stocker le résultat dans une variable, sinon la chaîne d'origine ne sera pas modifiée.

Vous pouvez également convertir une chaîne de caractères dans une variable en minuscules en utilisant une boucle ```for```. Voici un exemple de code :

```
String str = "Hello World!";
for (int i = 0; i < str.length(); i++) {
  str[i] = toLowerCase(str[i]); // convertit chaque caractère en minuscule
}
```

Cela modifiera la chaîne d'origine directement, vous n'avez donc pas besoin de créer une nouvelle variable pour stocker le résultat.

## Plongée en profondeur

Lorsque vous utilisez la fonction ```Arduino toLowerCase()```, il est important de noter qu'elle ne prend pas en compte les caractères accentués. Par exemple, la lettre "É" sera convertie en "e" plutôt qu'en "é".

De plus, cette fonction ne fonctionne que pour les caractères en minuscules et en majuscules de l'alphabet anglais. Les caractères spéciaux et les lettres accentuées seront toujours conservés dans leur forme d'origine.

Si vous avez besoin de prendre en compte les caractères spéciaux et les lettres accentuées dans votre conversion de chaîne en minuscules, vous pouvez utiliser une librairie externe comme la librairie ```<locale.h>```.

## Voir aussi

- [La documentation officielle de la fonction toLowerCase()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/)
- [Un tutoriel vidéo sur la conversion des chaînes en minuscules en programmation Arduino](https://www.youtube.com/watch?v=yLaRy3ZQvOw)
- [Un article sur l'utilisation de la librairie <locale.h> pour convertir en minuscules dans Arduino](https://forum.arduino.cc/index.php?topic=13838.0)

---

Markdown par Jérémy Verville