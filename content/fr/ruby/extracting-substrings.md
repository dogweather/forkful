---
title:    "Ruby: Extraction de sous-chaînes"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation Ruby, il est souvent nécessaire d'extraire des sous-chaînes de caractères d'une chaîne principale. Cela peut être utile pour traiter des informations spécifiques ou pour effectuer des manipulations sur des parties précises d'une chaîne. Dans cet article, nous allons expliquer comment extraire des sous-chaînes de manière efficace en utilisant le langage Ruby.

## Comment faire

Pour extraire des sous-chaînes en Ruby, nous pouvons utiliser la méthode `[]` ou la méthode `slice`, qui fonctionnent de la même manière. Elles prennent toutes les deux un index ou un intervalle en paramètre pour spécifier la portion de la chaîne à extraire.

```
# Exemple d'utilisation de la méthode `[]`
"Bonjour".[0] #=> "B"
"Bonjour".[2..4] #=> "njo"
```

```
# Exemple d'utilisation de la méthode `slice`
"Bonjour".slice(0) #=> "B"
"Bonjour".slice(2..4) #=> "njo"
```

Nous pouvons également utiliser la méthode `scan` pour extraire toutes les occurrences d'une sous-chaîne dans une chaîne principale. Cette méthode renvoie un tableau contenant toutes les sous-chaînes trouvées.

```
# Exemple d'utilisation de la méthode `scan`
"Je suis content de vivre à Paris.".scan("Paris") #=> ["Paris"]
```

## Plongée en profondeur

En plus des méthodes mentionnées précédemment, Ruby offre également des méthodes plus avancées pour extraire des sous-chaînes.

La méthode `sub` permet de remplacer la première occurrence d'une sous-chaîne spécifiée par une autre sous-chaîne. La méthode `gsub` permet quant à elle de remplacer toutes les occurrences. Ces deux méthodes renvoient une nouvelle chaîne modifiée sans modifier l'originale.

```
# Exemple d'utilisation de la méthode `sub`
"Bonjour".sub("n", "z") #=> "Bozjour"

# Exemple d'utilisation de la méthode `gsub`
"Bonjour".gsub("o", "a") #=> "Banjaar"
```

En utilisant des expressions régulières, nous pouvons également extraire des sous-chaînes précises en utilisant la méthode `match`, qui renvoie un objet `MatchData` contenant les informations sur la sous-chaîne trouvée.

```
# Exemple d'utilisation de la méthode `match`
"J'aime le chocolat".match(/chocolat/) #=> <MatchData "chocolat">
```

## Voir aussi

- [Ruby Documentation sur les chaînes](https://ruby-doc.org/core-2.7.1/String.html)
- [Tutorialspoint sur les sous-chaînes en Ruby](https://www.tutorialspoint.com/ruby/ruby_strings.htm)