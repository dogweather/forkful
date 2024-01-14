---
title:                "Python: Majuscule d'une chaîne de caractères"
simple_title:         "Majuscule d'une chaîne de caractères"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La capitalisation d'une chaîne de caractères est une étape importante dans la programmation Python car elle vous permet de formater votre texte selon vos besoins. Que vous cherchiez à mettre en valeur certains mots dans une phrase ou à normaliser la casse d'un ensemble de données, la capitalisation est un outil essentiel à connaître.

## Comment faire

La méthode de capitalisation en Python est simple et intuitive. Dans l'exemple ci-dessous, nous allons utiliser un code simple pour afficher la différence entre une chaîne de caractères non modifiée et une chaîne de caractères capitalisée :

```Python
string = "ceci est une chaîne de caractères"
print(string) # affiche "ceci est une chaîne de caractères"

string_capitalize = string.capitalize()
print(string_capitalize) # affiche "Ceci est une chaîne de caractères"
```

Comme vous pouvez le voir, la fonction `.capitalize()` transforme automatiquement la première lettre de la chaîne de caractères en majuscule. Vous pouvez également utiliser les méthodes `.upper()` et `.lower()` pour mettre en majuscule ou en minuscule toute la chaîne de caractères.

En outre, si vous voulez capitaliser chaque mot dans une phrase, vous pouvez utiliser la méthode `.title()` comme ceci :

```Python
string = "python est un langage de programmation"
print(string.title()) # affiche "Python Est Un Langage De Programmation"
```

## Plongée en profondeur

En plus des méthodes de base de capitalisation, Python offre également la possibilité de définir des règles plus personnalisées. Par exemple, la méthode `.swapcase()` inverse la casse de chaque lettre dans la chaîne de caractères :

```Python
string = "CeCi EsT uNe ChAiNe De CaRaCtÈrEs"
print(string.swapcase()) # affiche "cEcI eSt UnE cHaInE dE cArAcTèReS"
```

Vous pouvez également utiliser la méthode `.replace()` pour remplacer certaines lettres ou mots dans votre chaîne de caractères et les capitaliser en même temps :

```Python
string = "ceci est une chaine de caracteres"
print(string.replace("e", "E").capitalize()) # affiche "Ceci Est UnE ChainE dE CaractErEs"
```

Par ailleurs, si vous travaillez avec des chaînes de caractères codées en UTF-8, vous pouvez utiliser la méthode `.encode()` pour capitaliser correctement les caractères spéciaux :

```Python
string = "éòûà"
print(string.capitalize()) # affiche "Éòûà"

string_encoded = string.encode("utf-8")
print(string_encoded.capitalize().decode("utf-8")) # affiche "Éòûà"
```

## Voir aussi

Pour plus d'informations sur les méthodes de capitalisation en Python, vous pouvez consulter les ressources suivantes :

- Documentation officielle de Python sur les méthodes de chaînes de caractères : https://docs.python.org/3/library/stdtypes.html#string-methods
- Tutoriel interactif sur les chaînes de caractères en Python : https://www.learnpython.org/fr/String_Methods