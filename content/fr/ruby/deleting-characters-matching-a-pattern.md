---
title:                "Ruby: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

La suppression de caractères correspondant à un motif peut être un moyen efficace de nettoyer une chaîne de caractères ou de manipuler des données. Cela peut également être utile pour régler des problèmes de formatage ou de validation dans votre code.

## Comment faire

Voici un exemple de code Ruby pour supprimer tous les chiffres d'une chaîne de caractères :

````Ruby
chaine = "J'ai 25 ans"
chaine = chaine.gsub(/[0-9]/, "") # Remplace tous les chiffres par une chaîne vide
puts chaine # Résultat : "J'ai ans"
````

Notez l'utilisation de la méthode `gsub` qui permet de remplacer tous les éléments correspondant à un motif dans une chaîne de caractères.

Vous pouvez également utiliser des expressions régulières plus complexes pour cibler des motifs spécifiques :

````Ruby
chaine = "@johndoe"
chaine = chaine.gsub(/[!@#$%^&*()]/, "") # Supprime tous les symboles spéciaux
puts chaine # Résultat : "johndoe"
````

## Plongée en profondeur

La méthode `gsub` utilise en réalité des expressions régulières pour trouver les motifs à supprimer. Cela signifie que vous pouvez utiliser toute la puissance des expressions régulières pour manipuler vos chaînes de caractères.

Par exemple, vous pouvez utiliser des quantificateurs pour supprimer plusieurs occurrences d'un motif :

````Ruby
chaine = "haaahaaahaaahaa"
chaine = chaine.gsub(/ha{3}/, "") # Supprime toutes les occurrences de "haaa"
puts chaine # Résultat : "ha,ha,ha"
````

Ou encore, vous pouvez utiliser des groupes de capture pour réutiliser les éléments supprimés dans votre nouvelle chaîne :

````Ruby
chaine = "Bonjour, je m'appelle John Doe"
chaine = chaine.gsub(/(\w+)\s+John\s+(\w+)/, "Salut \\1, je suis \\2") # Remplace le nom complet par un salut
puts chaine # Résultat : "Salut Bonjour, je suis Doe"
````

En explorant les différentes possibilités des expressions régulières, vous pourrez supprimer efficacement tous les caractères indésirables de vos chaînes de caractères.

## Voir aussi

- [Documentation officielle de Ruby pour les expressions régulières](https://ruby-doc.org/core-2.7.1/Regexp.html)
- [Tutoriel RegexOne pour apprendre les expressions régulières](https://regexone.com/) 
- [Liste des métacaractères utilisables en expressions régulières](https://www.rexegg.com/regex-quickstart.html)