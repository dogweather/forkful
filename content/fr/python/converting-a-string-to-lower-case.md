---
title:                "Python: Transformer une chaîne de caractères en minuscule"
simple_title:         "Transformer une chaîne de caractères en minuscule"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Pourquoi

Lorsque vous travaillez avec des chaînes de caractères en Python, vous pourriez avoir besoin de les convertir en minuscules. Cela peut être utile pour traiter les entrées des utilisateurs de manière cohérente ou pour comparer des chaînes sans tenir compte de la casse. Dans cet article, nous allons vous montrer comment convertir une chaîne de caractères en minuscules en utilisant Python.

## Comment faire

Pour convertir une chaîne de caractères en minuscules en Python, vous pouvez utiliser la méthode intégrée "lower ()". Voici un exemple de code et sa sortie correspondante :

```Python
# Déclaration d'une chaîne de caractères
string = "BONJOUR TOUT LE MONDE!"

# Utilisation de la méthode lower()
new_string = string.lower()

# Affichage du résultat
print(new_string)
```
Sortie :

```
bonjour tout le monde!
```

Comme vous pouvez le voir, la méthode "lower()" a converti toutes les lettres en minuscules, y compris les lettres accentuées. Cela peut être très utile lorsque vous travaillez avec des chaînes de caractères qui contiennent des mots français.

## Plongée profonde

Il est important de noter que la méthode "lower()" ne modifie pas directement la chaîne d'origine, mais renvoie une nouvelle chaîne de caractères en minuscules. Cela signifie que si vous voulez enregistrer la chaîne convertie, vous devez l'assigner à une variable.

Il est également possible de convertir uniquement la première lettre d'une chaîne en minuscule en utilisant la méthode "capitalize ()". Voici un exemple :

```Python
# Déclaration d'une chaîne de caractères
string = "Bonjour tout le monde!"

# Utilisation de la méthode capitalize()
new_string = string.capitalize()

# Affichage du résultat
print(new_string)
```

Sortie :

```
Bonjour tout le monde!
```

Enfin, si vous souhaitez convertir une chaîne en majuscules, vous pouvez utiliser la méthode "upper ()".

### Voir aussi

- [Documentation officielle de la méthode lower()](https://docs.python.org/fr/3/library/stdtypes.html#str.lower)
- [Documentation officielle de la méthode capitalize()](https://docs.python.org/fr/3/library/stdtypes.html#str.capitalize)
- [Documentation officielle de la méthode upper()](https://docs.python.org/fr/3/library/stdtypes.html#str.upper)

Maintenant que vous savez comment convertir une chaîne de caractères en minuscules en utilisant Python, vous pouvez l'appliquer dans vos projets pour faciliter la manipulation de données textuelles. Merci d'avoir lu cet article et continuez à apprendre et à expérimenter avec Python!