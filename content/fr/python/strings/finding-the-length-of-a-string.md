---
date: 2024-01-20 17:48:34.362131-07:00
description: "La longueur d'une cha\xEEne, c'est combien de caract\xE8res elle contient.\
  \ Les programmeurs la mesurent pour valider des saisies, pour trancher des trucs,\
  \ ou\u2026"
lastmod: '2024-02-25T18:49:54.118429-07:00'
model: gpt-4-1106-preview
summary: "La longueur d'une cha\xEEne, c'est combien de caract\xE8res elle contient.\
  \ Les programmeurs la mesurent pour valider des saisies, pour trancher des trucs,\
  \ ou\u2026"
title: "Trouver la longueur d'une cha\xEEne de caract\xE8res"
---

{{< edit_this_page >}}

## What & Why?
La longueur d'une chaîne, c'est combien de caractères elle contient. Les programmeurs la mesurent pour valider des saisies, pour trancher des trucs, ou pour savoir où ils en sont.

## How to:
Utilisez `len()` pour trouver la longueur d'une chaîne. C'est simple comme bonjour :

```python
chaine = "Bonjour, le monde !"
longueur = len(chaine)
print(longueur)
```

Sortie :
```
20
```

Vous pouvez l'utiliser dans des conditions aussi :

```python
mot = "Bonjour"
if len(mot) > 5:
    print("C'est un mot assez long.")
```

Sortie :
```
C'est un mot assez long.
```

## Deep Dive
Historiquement, la fonction `len()` fait partie intégrante de Python. C'est la voie standard et elle colle à la philosophie "batteries incluses" du langage.

Alternatives? En principe, pas vraiment. Vous pourriez compter manuellement avec une boucle, mais pourquoi faire compliqué ?

```python
chaine = "Salut"
compteur = 0
for lettre in chaine:
    compteur += 1
print(compteur)
```

Sortie :
```
5
```

Mais c'est comme réinventer la roue – utilisez `len()`, c'est plus net.

Implémentation? `len()` fait appel à la méthode `__len__` d'un objet. Si vous créez vos propres objets, vous pouvez définir `__len__` pour contrôler le comportement de `len()` avec eux.

## See Also
Pour plus d'info sur `len()` et les séquences en Python :

- La doc officielle : [https://docs.python.org/3/library/functions.html#len](https://docs.python.org/3/library/functions.html#len)
- Un tuto sympa sur les chaînes : [https://realpython.com/python-strings/](https://realpython.com/python-strings/)
- Personnaliser `__len__` pour vos objets : [https://docs.python.org/3/reference/datamodel.html#object.__len__](https://docs.python.org/3/reference/datamodel.html#object.__len__)
