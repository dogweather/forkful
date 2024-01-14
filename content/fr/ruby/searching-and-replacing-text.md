---
title:    "Ruby: Recherche et remplacement de texte"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

# Pourquoi

La recherche et le remplacement de texte sont des tâches courantes dans la programmation Ruby. Cela peut être utile pour corriger des erreurs d'orthographe, mettre à jour des noms de variables ou même pour effectuer des tâches de refactoring. Dans cet article, nous allons vous montrer comment effectuer ces tâches simplement et efficacement.

# Comment faire

La méthode la plus simple pour effectuer une recherche et un remplacement de texte en Ruby est d'utiliser la méthode `gsub` (global substitution). Cette méthode prend deux paramètres : le premier est le texte à rechercher et le deuxième est le texte de remplacement. Par exemple, si nous avons une variable `texte` contenant la phrase "Bonjour à tous !", nous pouvons utiliser la méthode `gsub` pour remplacer "Bonjour" par "Salut" de la manière suivante :

```Ruby
texte.gsub("Bonjour", "Salut")
```

Le résultat de cette instruction sera la nouvelle variable `texte` contenant la phrase "Salut à tous !". Si nous voulons remplacer toutes les occurrences d'un mot, nous pouvons utiliser un regex (expression régulière) au lieu d'une chaîne de caractères. Par exemple, si nous voulons remplacer toutes les lettres "o" par des lettres "a", nous pouvons utiliser la méthode `gsub` de la manière suivante :

```Ruby
texte.gsub(/o/, "a")
```

Le résultat de cette instruction sera "Banjour à tauts !".

# Deep Dive

En utilisant les regex, nous pouvons effectuer des recherches et des remplacements plus complexes. Par exemple, si nous avons une liste de noms de villes comme "Paris", "Marseille" et "Lyon" et que nous voulons remplacer toutes les occurrences par "France", nous pouvons utiliser un regex de la manière suivante :

```Ruby
texte.gsub(/Paris|Marseille|Lyon/, "France")
```

Le pipe "|" est utilisé pour séparer les différentes options de remplacement. Nous pouvons également utiliser des paramètres supplémentaires pour modifier le comportement de la méthode `gsub`. Par exemple, nous pouvons utiliser le paramètre "i" pour ne pas tenir compte de la casse. Ainsi, si notre texte contient "paris" en minuscules, il sera également remplacé par "France".

# Voir aussi

- [Ruby String#gsub documentation](https://ruby-doc.org/core-2.7.2/String.html#method-i-gsub)
- [Mastering Regular Expressions in Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Ruby Cheat Sheet: Regex Edition](https://www.rubyinside.com/cheat-sheet-ruby-regular-expressions-1233.html)