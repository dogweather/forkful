---
title:    "Python: Capitaliser une chaîne de caractères"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La capitalisation d'une chaîne de caractères est une opération courante en programmation qui consiste à mettre en majuscule la première lettre de chaque mot dans une chaîne. Cette pratique est souvent utilisée pour rendre le texte plus lisible ou pour respecter les conventions de nommage dans un code.

## Comment faire

Il existe plusieurs façons de capitaliser une chaîne en Python. Voici quelques exemples en utilisant différentes méthodes :

```Python
# Utilisation de la méthode capitalize()
phrase = "bonjour tout le monde"
print(phrase.capitalize()) 
# Output: Bonjour tout le monde

# Utilisation de la méthode title()
phrase = "bonjour tout le monde"
print(phrase.title()) 
# Output: Bonjour Tout Le Monde

# Utilisation de la méthode upper()
phrase = "bonjour tout le monde"
print(phrase.upper()) 
# Output: BONJOUR TOUT LE MONDE
```

Il est important de noter que ces méthodes ne modifient pas la chaîne originale mais renvoient une copie de la chaîne capitalisée. Vous pouvez également utiliser une boucle et la méthode de slicing pour capitaliser une chaîne caractère par caractère. 

## Plongée en profondeur

La méthode capitalize() ne modifie que la première lettre de la chaîne, tandis que la méthode title() met en majuscule la première lettre de chaque mot dans la chaîne. La méthode upper() met quant à elle en majuscule toutes les lettres de la chaîne, ce qui peut être utile si vous souhaitez capitaliser une chaîne entière. 

Il est important de noter que ces méthodes ne fonctionnent que pour les lettres de l'alphabet. Les caractères spéciaux, les chiffres ou les espaces ne seront pas modifiés. De plus, si une lettre est déjà en majuscule, elle ne sera pas modifiée par ces méthodes.

## Voir aussi

- [Documentation officielle Python pour la méthode capitalize()](https://docs.python.org/fr/3/library/stdtypes.html#str.capitalize)
- [Documentation officielle Python pour la méthode title()](https://docs.python.org/fr/3/library/stdtypes.html#str.title)
- [Documentation officielle Python pour la méthode upper()](https://docs.python.org/fr/3/library/stdtypes.html#str.upper)