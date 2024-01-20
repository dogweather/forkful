---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Go: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Trouver la longueur d'une chaîne en Python

## Quoi & Pourquoi?
Déterminer la longueur d'une chaîne (en anglais: "string") signifie compter le nombre de caractères dans la chaîne. Les programmeurs font cela pour manipuler et gérer efficacement les données textuelles dans leurs programmes.

## Comment faire:
Voici comment vous pouvez trouver la longueur d'une chaîne en Python.

```Python 
chaine = 'Bonjour le monde!'
print(len(chaine))
```

Ceci donnera la sortie:

```Python 
17
```

Dans l'exemple ci-dessus, `len` est une fonction intégrée en Python qui renvoie la longueur (le nombre de caractères) de la chaîne donnée.

## Plongée en profondeur
En ce qui concerne la longueur de la chaîne en Python, il n'y a pas beaucoup de contexte historique. Cependant, il faut savoir que `len` est une fonction intégrée dans Python dès sa première version.

Au lieu d'utiliser `len`, on peut aussi utiliser une boucle `for` pour compter chaque caractère dans une chaîne, mais cela consomme plus de temps et de ressources.

```Python 
chaine = 'Bonjour le monde!'
compteur = 0
for i in chaine:
    compteur += 1
print(compteur)
```

Ceci donnera également sortie `17`, comme dans l'exemple précédent.

Côté mise en œuvre, la fonction `len` en Python utilise la méthode de la classe de l'objet passé. Par exemple, pour les chaînes, elle renvoie le nombre de caractères.

## Voir aussi
Pour plus d'informations sur les chaînes en Python, visitez les liens ci-dessous:

- [Documentation Python sur les chaînes de caractères](https://docs.python.org/fr/3/library/stdtypes.html#text-sequence-type-str)
- [Chaînes de caractères - Tutoriel Python](https://docs.python.org/fr/3/tutorial/introduction.html#strings)