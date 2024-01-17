---
title:                "Conversion d'une chaîne de caractères en minuscules"
html_title:           "Python: Conversion d'une chaîne de caractères en minuscules"
simple_title:         "Conversion d'une chaîne de caractères en minuscules"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?
Convertir une chaîne de caractères en minuscule est une opération couramment utilisée par les programmeurs pour uniformiser les données et faciliter leur traitement. Cela consiste à changer toutes les lettres majuscules d'une chaîne en minuscules, tout en conservant les autres caractères.

## Comment:
```Python
string = "Bonjour à TOUS"
lowercase_string = string.lower()

print(lowercase_string)
```

Output: "bonjour à tous"

## Plongée en profondeur:
La conversion de chaîne de caractères en minuscule est apparue dans les langages de programmation depuis les années 1960, avec l'avènement des ordinateurs. La principale alternative à cette opération est la mise en place de conditions pour traiter les lettres majuscules et minuscules différemment. En Python, cette opération est implémentée à l'aide de la méthode "lower()", qui peut être utilisée sur n'importe quelle chaîne de caractères.

## Voir aussi:
Pour en savoir plus sur la conversion de chaîne de caractères en minuscule en Python, consultez la documentation officielle : https://docs.python.org/fr/3/library/stdtypes.html#str.lower