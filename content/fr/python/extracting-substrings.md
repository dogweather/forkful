---
title:                "Extraction de sous-chaînes"
html_title:           "Arduino: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Qu'est-ce Que C'est & Pourquoi?

Extraire une sous-chaîne est le processus d'obtenir un ensemble spécifique de caractères à partir d'une chaîne. On l'utilise pour isoler des informations précises dans un texte, ce qui est essentiel dans l'analyse de données, le web scraping et bien d'autres tâches.

## Comment Faire?

La méthode la plus simple pour extraire des sous-chaînes en Python est d'utiliser l'opératrice de découpage (slicing).

```Python
chaine = "Bonjour, je suis Python!"

# Récupérer les 7 premiers caractères
print(chaine[:7])
# Sortie: Bonjour

# Récupérer la sous-chaîne entre le 9ème et le 11ème caractères
print(chaine[8:11])
# Sortie: je
```

## Plongée en Profondeur

L'opératrice de découpage a été introduite dans Python depuis sa création, offrant un moyen facile d'extraire des sous-chaînes directement. Cependant, Python offre plus qu'une seule façon de réaliser la même tâche. Les méthodes alternatives incluent les fonctions de la bibliothèque standard telle que find() et substring().

- `str.find(str, beg = 0, end = len(string))`: C'est une méthode qui détermine si la chaîne str se produit dans une chaîne et renvoie l'indice du premier caractère de la première occurrence.

- `str.split(sep=None, maxsplit=-1)`: C'est une autre méthode qui divise la chaîne en sous-chaînes si elle trouve des instances du séparateur sep.

Quant à la mise en œuvre, Python stocke les chaînes comme des tableaux de caractères. L'opératrice de découpage crée simplement un nouvel ensemble de références à ces caractères, sans dupliquer les caractères eux-mêmes.

## Voir Aussi

Pour plus d'informations sur l'extraction de sous-chaînes en Python, consulter ces liens complémentaires :

- Python String (W3Schools): https://www.w3schools.com/python/python_strings.asp
- Python String find() Method (GeeksforGeeks): https://www.geeksforgeeks.org/python-string-find/
- Python String split() Method (Programiz): https://www.programiz.com/python-programming/methods/string/split