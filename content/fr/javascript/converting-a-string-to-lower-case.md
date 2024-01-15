---
title:                "Transformation d'une chaîne en minuscules"
html_title:           "Javascript: Transformation d'une chaîne en minuscules"
simple_title:         "Transformation d'une chaîne en minuscules"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi il serait important de convertir une chaîne de caractères en minuscules en Javascript. La réponse est simple : cela peut être utile lors de la manipulation de données utilisateur ou de la comparaison de chaînes pour s'assurer qu'elles sont identiques, indépendamment de la casse.

## Comment faire

Pour convertir une chaîne de caractères en minuscules en Javascript, vous pouvez utiliser la méthode `toLowerCase()`. Voici un exemple de code et la sortie obtenue :

```Javascript
let str = "Bonjour tout le monde !";
console.log(str.toLowerCase()); // affiche "bonjour tout le monde !"
```

La méthode `toLowerCase()` renvoie une nouvelle chaîne en minuscules sans modifier la chaîne d'origine. Ainsi, si vous souhaitez enregistrer la valeur en minuscules, vous devrez la stocker dans une nouvelle variable.

```Javascript
let str = "J'aime les Frites";
let strMin = str.toLowerCase();
console.log(strMin); // affiche "j'aime les frites"
```

Il est également important de noter que la méthode `toLowerCase()` ne fonctionne que sur les caractères ASCII. Cela signifie qu'elle ne convertira pas les caractères avec des accents ou des lettres spéciales en minuscules. Pour cela, vous devrez utiliser une librairie de traitement de chaînes de caractères ou une méthode plus avancée.

## Plongée en profondeur

La méthode `toLowerCase()` utilise l'algorithme Unicode pour convertir les caractères en minuscules. Elle fonctionne en prenant le caractère d'origine et en le comparant à un tableau de caractères spécifiques qui correspondent à sa version en minuscules. La méthode tient également compte des règles de langue spécifiques, telles que les exceptions pour les lettres majuscules suivant certains caractères, comme "i" avant "j".

Il convient également de noter que la méthode `toLowerCase()` ne fonctionne que sur les chaînes de caractères. Si vous essayez de l'utiliser sur un autre type de données, comme un nombre ou un booléen, vous obtiendrez une erreur.

## Voir aussi

- [Méthode toLowerCase() sur MDN](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/toLowerCase)
- [Librairie de traitement de chaînes de caractères Lodash](https://lodash.com/docs/4.17.15#lowerCase)

Maintenant que vous savez comment convertir une chaîne de caractères en minuscules en Javascript, vous pourrez utiliser cette méthode dans vos projets pour manipuler et comparer des chaînes avec plus de facilité. N'hésitez pas à vous référer à la documentation et à expérimenter avec d'autres méthodes de manipulation de chaînes pour approfondir vos connaissances en Javascript. Happy coding!