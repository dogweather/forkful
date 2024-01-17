---
title:                "Concaténation de chaînes"
html_title:           "Javascript: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que & Pourquoi?

La concaténation de chaînes en Javascript signifie combiner plusieurs chaines de caractères en une seule. Les programmeurs font cela pour créer des phrases ou des phrases plus longues à partir de petits morceaux de texte.

## Comment faire:

### Exemple 1:

```Javascript
let phrase = "Bonjour";
let nom = "Jean";
let greeting = phrase + " " + nom;
console.log(greeting);
```

### Résultat 1:
```
Bonjour Jean
```

### Exemple 2:

```Javascript
let nombre = 52;
let message = "J'ai " + nombre + " ans.";
console.log(message);
```

### Résultat 2:
```
J'ai 52 ans.
```

## Plongée Profonde:

La concaténation de chaînes a été introduite dans les premières versions de Javascript en 1995. À l'époque, elle était couramment utilisée pour créer des pop-ups et des alertes dans les navigateurs. Aujourd'hui, il existe d'autres méthodes pour combiner des chaînes de caractères, telles que l'utilisation de modèles littéraux ou la méthode .concat(). Cependant, la concaténation reste une méthode simple et efficace pour composer des chaînes de caractères en Javascript.

## Voir Aussi:

Pour en savoir plus sur la concaténation de chaînes en Javascript, consultez ces sources: 

- [Documentation MDN sur la concaténation de chaînes en Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/String_Operators)
- [Article sur différents moyens de combiner des chaînes de caractères en Javascript](https://www.techiedelight.com/concatenate-strings-javascript/)