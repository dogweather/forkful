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

## Pourquoi

Si vous êtes un programmeur en herbe ou expérimenté, vous avez probablement déjà rencontré une situation où vous avez dû modifier une grande quantité de texte dans votre code. Peut-être que vous avez renommé une variable ou que vous avez changé un mot-clé dans toutes vos fonctions. Heureusement, il existe une solution simple pour cela: la recherche et le remplacement de texte en Javascript.

## Comment faire

Pour effectuer une recherche et un remplacement, nous utiliserons la méthode `replace()` disponible pour les chaînes de caractères en Javascript. Voici un exemple de code pour remplacer tous les caractères "a" par des "o":

```Javascript
let phrase = "Ma maman m'a dit un jour que j'étais un as en Javascript.";
let nouvellePhrase = phrase.replace(/a/g, "o");
console.log(nouvellePhrase);
```

Résultat:

```
Mo momon m'o dit un jour que j'étois un os en Jovoscript.
```

Dans cet exemple, nous avons utilisé une expression régulière pour indiquer que nous voulons remplacer toutes les occurrences de "a" (indiqué par `/a/`) et le modificateur "g" pour indiquer que nous voulons le faire sur toute la chaîne de caractères (et pas seulement la première occurrence). Ensuite, nous avons spécifié le texte de remplacement "o".

Vous pouvez également utiliser cette méthode pour remplacer du texte par une variable ou une expression. Voici un autre exemple:

```Javascript
let phrase = "Le résultat final est : XX";
let resultat = 42;
let nouvellePhrase = phrase.replace("XX", resultat);
console.log(nouvellePhrase);
```

Résultat:

```
Le résultat final est : 42 
```

La méthode `replace()` peut également être utilisée sur une chaîne de caractères plusieurs fois. Voyons comment remplacer plusieurs mots dans une phrase en utilisant un tableau:

```Javascript
let phrase = "Les chats mangent du poisson.";
let motsAremplacer = ["chats", "poisson"];
let nouveauxMots = ["chiens", "viande"];
let nouvellePhrase = phrase;

for (let i = 0; i < motsAremplacer.length; i++) {
  nouvellePhrase = nouvellePhrase.replace(motsAremplacer[i], nouveauxMots[i]);
}

console.log(nouvellePhrase);
```

Résultat:

```
Les chiens mangent de la viande.
```

## Plongée en profondeur

La méthode `replace()` permet également d'utiliser des expressions régulières avec des groupes de capture pour effectuer des substitutions plus complexes. Par exemple, pour remplacer des URLs avec des balises de lien, on pourrait utiliser le code suivant:

```Javascript
let phrase = "Regardez ce lien: https://www.example.com et aussi celui-ci: https://www.otherexample.com";
let nouvellePhrase = phrase.replace(/(https:\/\/www\.[a-z]+\.com)/g, '<a href="$1">$1</a>');
console.log(nouvellePhrase);
```

Résultat:

```
Regardez ce lien: <a href="https://www.example.com">https://www.example.com</a> et aussi celui-ci: <a href="https://www.otherexample.com">https://www.otherexample.com</a>
```

Cependant, il est important de noter que la méthode `replace()` ne modifie pas la chaîne d'origine, mais retourne une nouvelle chaîne avec les modifications. Pour modifier la chaîne d'origine, vous devez assigner la nouvelle chaîne à la variable d'origine.

## Voir aussi

- [La documentation officielle de la méthode replace()](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/replace)
- [Un tutoriel complet sur les expressions régulières en Javascript](https://www.regular-expressions.info/javascript.html)
- [Un outil en ligne pour tester vos expressions régulières](https://regex101.com/)