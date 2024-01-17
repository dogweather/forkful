---
title:                "Recherche et remplacement de texte"
html_title:           "Javascript: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

La recherche et le remplacement de texte sont des tâches courantes dans la programmation. Il s'agit simplement de trouver une partie spécifique du texte dans un document ou un code source, puis de le remplacer par un autre texte. Les programmeurs le font pour modifier ou améliorer rapidement leur code sans avoir à le modifier manuellement.

## Comment le faire:

Voici un exemple de code en Javascript pour rechercher et remplacer du texte dans une chaîne de caractères :

```Javascript
let message = "Bonjour, je suis un programmeur en Javascript!";
let newMessage = message.replace("Javascript", "Python");
console.log(newMessage);
```

L'output sera : "Bonjour, je suis un programmeur en Python!"

Pour rechercher et remplacer du texte dans un document HTML, vous pouvez utiliser la méthode `querySelectorAll` suivie de la méthode `forEach` :

```Javascript
let paragraphs = document.querySelectorAll("p");
paragraphs.forEach(paragraph => {
  let text = paragraph.textContent;
  let newText = text.replace("JavaScript", "Python");
  paragraph.textContent = newText;
});
```

## Plongée en profondeur:

La recherche et le remplacement de texte ont été rendus populaires par les éditeurs de texte comme Vim et Emacs, qui permettaient aux utilisateurs de remplacer du texte en utilisant des expressions régulières. Cependant, il existe maintenant des outils plus conviviaux et plus puissants pour effectuer ces tâches, tels que les extensions de recherche et de remplacement de texte pour les éditeurs de code. Alternativement, les moteurs de recherche tels que Google utilisent également des algorithmes de recherche et de remplacement pour trouver des résultats pertinents.

## A voir aussi:

Voici quelques ressources supplémentaires pour en savoir plus sur la recherche et le remplacement de texte en programmation :

- [Documentation Javascript sur la méthode "replace"](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/replace)
- [Une introduction aux expressions régulières en Javascript](https://www.regular-expressions.info/javascript.html)
- [Extension de recherche et de remplacement pour VSCode](https://marketplace.visualstudio.com/items?itemName=jakob101.search-and-replace)
- [Comment fonctionne la recherche et le remplacement dans les moteurs de recherche](https://webmasters.googleblog.com/2008/09/dynamic-snippets-ever-changed-content.html)