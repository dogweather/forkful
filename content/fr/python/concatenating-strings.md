---
title:                "Concaténation de chaînes"
html_title:           "C: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/concatenating-strings.md"
---

{{< edit_this_page >}}

# Concaténation de Chaînes en Python: Un Guide Pratique

## Quoi & Pourquoi?
La concaténation de chaînes est l'acte de joindre deux ou plusieurs chaînes de caractères en une seule. Les programmeurs la mettent en œuvre pour manipuler efficacement les données et les afficher dans des formats spécifiques.

## Comment faire:
En Python, c'est simple comme un jeu d'enfant. Voici quelques exemples:

```Python
# Utiliser l'opérateur '+'
prenom = 'Marie'
nom = 'Dubois'
print(prenom + ' ' + nom)
```
Sortie:
```
Marie Dubois
```

```Python
# utiliser la fonction .join()
phrases = ['Salut', 'tout', 'le', 'monde']
print(' '.join(phrases))
```
Sortie:
```
Salut tout le monde
```

## D'Un Coup D'oeil
Historiquement, la concaténation de chaînes est une fonction fondamentale de la plupart des langages de programmation, pas seulement Python. Cependant, il existe toujours des alternatives. Par exemple, en Python, en plus de '+' et de '.join()', vous pouvez également utiliser l'interpolation de chaînes:

```Python
age = 25
message = f"J'ai {age} ans."
print(message)
```
Sortie:
```
J'ai 25 ans.
```
C'est un moyen propre et efficace de concaténer et de formatter les chaînes en même temps.

## Voir Aussi
Pour aller plus loin, consultez les ressources suivantes:
- [La documentation de Python sur les chaînes de caractères](https://docs.python.org/3/tutorial/introduction.html#strings)
- [Format des chaînes en Python: Tutoriel Real Python](https://realpython.com/python-string-formatting/)