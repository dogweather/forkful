---
title:                "Extraction de sous-chaînes"
html_title:           "Ruby: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

L'extraction de sous-chaînes fait référence au fait de récupérer un morceau spécifique d'une chaîne de caractères dans un programme Ruby. Les programmeurs le font souvent pour extraire des informations précises d'une chaîne, comme un numéro de téléphone ou une adresse e-mail.

## Comment faire:

Voici quelques exemples de code pour extraire une sous-chaîne à partir d'une chaîne donnée en utilisant la méthode ```.slice()``` :

```
# Exemple 1:
str = "Bienvenue sur mon site web!"
puts str.slice(10, 5) # Output: "mon s"

# Exemple 2:
str = "123-456-789"
puts str.slice(4..6) # Output: "456"

# Exemple 3:
str = "ruby est un langage très puissant"
puts str.slice(-8..-1) # Output: "puissant"
```

## Plongée en profondeur:

L'extraction de sous-chaînes est un concept courant en programmation, avec des méthodes similaires disponibles dans d'autres langages de programmation tels que Python et Java. Les alternatives à la méthode ```.slice()``` en Ruby incluent ```.substring()``` et ```.split()```.

En termes d'implémentation, les sous-chaînes sont souvent manipulées en utilisant des index correspondant à chaque caractère dans la chaîne, avec l'ajout ou la soustraction de valeurs pour sélectionner la portion souhaitée.

## À voir également:

Pour en savoir plus sur l'extraction de sous-chaînes en Ruby, consultez la documentation officielle de Ruby sur les méthodes de manipulation de chaînes. Vous pouvez également explorer différentes façons de traiter les chaînes en utilisant les fonctions équivalentes dans d'autres langages de programmation.