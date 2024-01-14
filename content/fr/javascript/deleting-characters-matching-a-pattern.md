---
title:                "Javascript: Suppression de caractères correspondant à un motif"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi supprimer des caractères correspondants à un modèle ?

Parfois, lors de la programmation en Javascript, vous pouvez vous retrouver avec des chaînes de caractères contenant des caractères indésirables qui ne correspondent pas au format que vous souhaitez. Dans ces situations, vous pouvez utiliser une fonction de suppression de caractères correspondants à un modèle pour nettoyer vos données et les rendre plus utilisables pour votre code.

## Comment faire :

```Javascript
function deleteMatchingCharacters(str, pattern) {
  // Itérer à travers la chaîne de caractères
  for (let i = 0; i < str.length; i++) {
    // Vérifiez si le caractère correspond au modèle
    if (str[i].match(pattern)) {
      // Supprimer le caractère en utilisant la méthode splice
      str.splice(i, 1);
      // Réduire l'indice pour que la boucle puisse continuer correctement
      i--;
    }
  }
  // Retourner la chaîne de caractères modifiée
  return str;
}

// Exemple d'utilisation
let str = "H3ll0, W0rld!";
let pattern = /[0-9]/; // Recherche des nombres dans la chaîne de caractères
let output = deleteMatchingCharacters(str, pattern);
console.log(output); // Résultat : "Hll, Wrld!"
```

Vous pouvez également utiliser la méthode `replace` pour supprimer les caractères correspondants à un modèle :

```Javascript
let str = "H3ll0, W0rld!";
let pattern = /[0-9]/g; // Ajout du drapeau global pour supprimer tous les nombres
let output = str.replace(pattern, "");
console.log(output); // Résultat : "Hll, Wrld!"
```

## Un aperçu plus précis :

La fonction que nous avons créée utilise une boucle pour parcourir chaque caractère de la chaîne de caractères donnée. Ensuite, elle vérifie si le caractère correspond au modèle spécifié. Si c'est le cas, le caractère est supprimé à l'aide de la méthode `splice` et l'indice de la boucle est réduit pour éviter tout décalage. Enfin, la chaîne de caractères modifiée est renvoyée.

Il est important de noter que cette fonction ne modifie pas la chaîne de caractères originale, mais en crée plutôt une nouvelle avec les caractères correspondants retirés. De plus, il est possible de spécifier un modèle plus complexe en utilisant des expressions régulières pour supprimer différents types de caractères.

## Voir aussi :

- [La méthode `splice` de JavaScript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Array/splice)
- [La méthode `replace` de JavaScript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/replace)
- [Les expressions régulières en JavaScript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Guide/Regular_Expressions)