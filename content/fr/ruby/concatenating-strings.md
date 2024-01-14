---
title:                "Ruby: Concaténer des chaînes de caractères."
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

# Pourquoi

La concaténation de chaînes de caractères est une méthode couramment utilisée en programmation Ruby pour combiner des chaînes de caractères en une seule. Cela peut être utile pour créer des messages personnalisés, des noms de fichiers dynamiques, ou pour manipuler des données texte.

# Comment faire

La concaténation de chaînes en Ruby est très simple grâce à l'utilisation de l'opérateur de concaténation (+) ou de la méthode .concat(). Voici un exemple:

```
prenom = "Jean"
nom = "Dupont"
puts prenom + nom
# Output: JeanDupont
```

Nous pouvons également utiliser la méthode .concat() pour concaténer plusieurs chaînes de caractères:

```
pays = "France"
ville = "Paris"
rue = "Rue du Faubourg Saint-Honoré"
puts "L'adresse complète est: " + pays.concat(", " + ville, ", " + rue)
# Output: L'adresse complète est: France, Paris, Rue du Faubourg Saint-Honoré
```

Il est également possible d'utiliser l'opérateur de concaténation pour ajouter des espaces entre les chaînes:

```
prenom = "Marie"
nom = "Martin"
puts prenom + " " + nom
# Output: Marie Martin
```

# Plongée en profondeur

Ruby fournit également une méthode .prepend() qui permet de concaténer une chaîne de caractères au début d'une autre. Elle peut être utile pour ajouter un préfixe à une chaîne:

```
nom = "Smith"
puts nom.prepend("Monsieur ")
# Output: Monsieur Smith
```

Nous pouvons également utiliser l'opérateur "+" ou la méthode .concat() avec des variables numériques pour les convertir en chaînes de caractères et les concaténer avec d'autres chaînes:

```
nombre = 10
puts "Le nombre est " + nombre.to_s
# Output: Le nombre est 10
```

# Voir aussi

- [Documentation Ruby sur les chaînes](https://ruby-doc.org/core-3.0.0/String.html)
- [Tutoriel sur les chaînes en Ruby](https://www.w3schools.in/ruby-tutorial/strings/)
- [Documentation officielle sur la concaténation de chaînes en Ruby](https://ruby-doc.org/core-3.0.0/String.html#method-i-2B)