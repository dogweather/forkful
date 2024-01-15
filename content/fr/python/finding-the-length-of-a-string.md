---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Python: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi vous devriez vous intéresser à trouver la longueur d'une chaîne de caractères (ou string en anglais). Eh bien, cela peut être utile dans de nombreux cas, tels que:

- Vérifier si un mot saisi par l'utilisateur respecte une limite de caractères définie
- Compter le nombre de caractères dans une phrase pour l'utiliser dans un algorithme de traitement de texte
- Valider la longueur d'un mot de passe pour assurer la sécurité des comptes utilisateur

Maintenant que vous avez une idée de l'utilité de cette tâche, voyons comment la réaliser en Python.

## Comment faire

```Python
# Déclaration d'une chaîne de caractères
ma_chaine = "Bonjour"

# Utilisation de la fonction len() pour trouver la longueur de la chaîne
longueur = len(ma_chaine)

# Affichage du résultat
print(longueur) # Output: 7
```

Comme vous pouvez le voir, la fonction len() est utilisée pour trouver la longueur d'une chaîne de caractères en Python. Il suffit de passer la chaîne en tant que paramètre à la fonction et elle renvoie le nombre de caractères dans la chaîne. Notez que les espaces et les caractères spéciaux comptent également comme des caractères.

Vous pouvez également utiliser cette fonction sur les chaînes de caractères saisies par l'utilisateur:

```Python
# Demander à l'utilisateur de saisir une chaîne de caractères
utilisateur = input("Veuillez saisir une phrase: ")

# Utilisation de la fonction len() pour trouver la longueur de la chaîne
longueur = len(utilisateur)

# Affichage du résultat
print(longueur)
```

Maintenant que vous savez comment trouver la longueur d'une chaîne en Python, explorons un peu plus en profondeur comment cela fonctionne.

## Plongée en profondeur

La fonction len() en Python renvoie en fait le nombre d'éléments dans un objet. Pour les chaînes de caractères, ces éléments sont simplement les caractères individuels. Cela signifie que vous pouvez également utiliser la fonction len() sur d'autres types d'objets en Python, tels que les listes et les tuples.

Lorsque vous utilisez la fonction len() sur une chaîne de caractères, elle parcourt la chaîne et compte chaque caractère jusqu'à ce qu'elle atteigne la fin de la chaîne. Cela signifie que plus la chaîne est longue, plus la fonction len() prendra de temps à s'exécuter.

S'il y a des caractères spéciaux ou des accents dans votre chaîne de caractères, la fonction len() les comptera également. Cela peut parfois entraîner des différences entre la longueur réelle d'une chaîne (en termes de nombre de caractères visibles) et le résultat renvoyé par la fonction len().

Maintenant que vous avez une compréhension plus approfondie de la fonction len() en Python, vous pouvez l'utiliser de manière efficace dans votre code.

## Voir aussi

- [Documentation officielle Python sur la fonction len()](https://docs.python.org/fr/3/library/functions.html#len)
- [Guide complet des opérations sur les chaînes en Python](https://www.w3schools.com/python/python_strings.asp)