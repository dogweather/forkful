---
title:    "TypeScript: Rechercher et remplacer du texte"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Pourquoi

Lorsque vous travaillez sur un projet de développement TypeScript, il est courant de devoir effectuer des modifications de texte dans votre code. Ceci peut être pour corriger des fautes d'orthographe, modifier des noms de variables ou même remplacer un morceau de code entier. Dans cet article, nous allons explorer comment rechercher et remplacer du texte dans TypeScript.

# Comment faire

La première étape pour rechercher et remplacer du texte dans TypeScript est de déterminer quelles parties de votre code doivent être modifiées. Pour cela, nous pouvons utiliser la méthode `indexOf()` qui renvoie l'index de la première occurrence d'un mot ou d'une chaîne de caractères dans une autre chaîne. Voici un exemple de code qui utilise `indexOf()` pour trouver l'index d'une chaîne de caractères dans une autre :

```TypeScript
const str1 = "Bonjour le monde !";
console.log(str1.indexOf("monde"));
```

Lorsque vous exécutez ce code, il vous affichera l'index de la première occurrence de "monde" dans la chaîne de caractères "Bonjour le monde !". Dans cet exemple, l'index renvoyé sera 12 car la chaîne de caractères "monde" commence à l'index 12 dans la chaîne de caractères "Bonjour le monde !".

Maintenant que nous avons l'index du texte que nous voulons remplacer, nous pouvons utiliser la méthode `substring()` pour créer une nouvelle chaîne de caractères avec les parties que nous voulons garder et celles que nous voulons remplacer. Voici un exemple de code qui utilise `substring()` pour remplacer le mot "monde" par "univers" dans la chaîne de caractères "Bonjour le monde !" :

```TypeScript
let str2 = "Bonjour le monde !";
str2 = str2.substring(0, 12) + "univers" + str2.substring(17);
console.log(str2);
```

En exécutant ce code, vous verrez que la chaîne de caractères "Bonjour le monde !" a été modifiée en "Bonjour l'univers !", en remplaçant "monde" par "univers".

# Plongée en profondeur

Il existe différentes méthodes pour rechercher et remplacer du texte en TypeScript, et cela dépendra de vos besoins spécifiques. Vous pouvez utiliser des méthodes telles que `replace()`, `split()` ou même des expressions régulières pour effectuer des modifications de texte plus complexes.

En outre, lorsque vous travaillez avec des objets, vous pouvez utiliser `Object.keys()` pour obtenir une liste des clés d'un objet, puis parcourir cette liste pour trouver et remplacer du texte dans les valeurs correspondantes.

# Voir aussi

- [Documentation TypeScript sur les méthodes de chaîne de caractères](https://www.typescriptlang.org/docs/handbook/strings.html)
- [Guide sur les expressions régulières en TypeScript](https://www.tutorialspoint.com/typescript/typescript_regular_expressions.htm)
- [Exemples de rechercher/remplacer en TypeScript](https://www.geeksforgeeks.org/ts-strings-replace-method/#:~:text=%7B%7D%29%20method,characters%20with%20new%20string%20characters.&text=replace(%2F2350%2F%2C%20'3254%3F%20'%29%20%2F%2F%20prints%20G,letters%20with%20new%20string%20characters.)