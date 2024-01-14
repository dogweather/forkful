---
title:                "Javascript: Extraction de sous-chaînes"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

# Pourquoi

Si vous êtes un programmeur JavaScript, vous avez probablement rencontré un problème où vous aviez besoin de rechercher et d'extraire une partie spécifique d'une chaîne de caractères. Cela pourrait être des éléments tels qu'un nom, un numéro de téléphone, ou une adresse e-mail. Dans ces cas, il est utile de savoir comment extraire des sous-chaînes de caractères.

# Comment faire

Voici un exemple de code qui utilise la méthode `substring()` pour extraire une sous-chaîne à partir d'une chaîne de caractères :

```Javascript
let fullName = "Jean Dupont";
let firstName = fullName.substring(0, 4);
console.log(firstName); // Output: Jean
```

La méthode `substring()` prend deux paramètres : l'index de départ et l'index de fin de la sous-chaîne à extraire. Dans notre exemple, nous avons commencé à l'index 0 (le premier caractère) et nous avons fini à l'index 4 (le caractère avant le 4ème index).

Si vous souhaitez extraire une sous-chaîne à partir d'un index spécifique jusqu'à la fin de la chaîne, vous pouvez simplement ne spécifier qu'un seul paramètre comme ceci :

```Javascript
let email = "johndoe@email.com";
let domain = email.substring(9);
console.log(domain); // Output: email.com
```

# Plongée en profondeur

Il est important de noter que la méthode `substring()` ne modifie pas la chaîne d'origine, mais renvoie plutôt une nouvelle chaîne avec la sous-chaîne extraite. De plus, les index de la méthode commencent à zéro, donc le premier caractère est à l'index 0, le deuxième à l'index 1, et ainsi de suite.

En plus de `substring()`, il existe d'autres méthodes utiles pour extraire des sous-chaînes de caractères en JavaScript, telles que `slice()` et `substr()`. Toutefois, `substring()` est souvent la méthode préférée car elle a une syntaxe plus simple et plus facile à comprendre.

# Voir aussi

- Documentation officielle de la méthode `substring()` : https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/substring
- Comparaison entre les méthodes `substring()`, `slice()` et `substr()` : https://www.educative.io/edpresso/the-differences-between-substr-slice-and-substring-in-javascript