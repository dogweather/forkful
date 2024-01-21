---
title:                "Interpolation de chaînes de caractères"
date:                  2024-01-20T17:51:27.661886-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolation de chaînes de caractères"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)

L'interpolation de chaînes permet d'insérer des valeurs au sein d'une chaîne de caractères. Les programmeurs l'utilisent pour créer du texte dynamique facile à lire et à maintenir.

## How to: (Comment faire :)

```python
# Utilisation de f-strings pour l'interpolation
nom = "Amélie"
age = 27
message = f"Bonjour, je suis {nom} et j'ai {age} ans."
print(message)
```

Sortie :
```
Bonjour, je suis Amélie et j'ai 27 ans.
```

## Deep Dive (Plongée en Profondeur)

Historiquement, Python a connu plusieurs méthodes d'interpolation. `%` a été les premiers outils d'interpolation, appelés opérateurs de formatage. Avec `.format()`, introduit en Python 2.6, le code devenait plus clair. Python 3.6 a amené les f-strings, qui sont à la fois plus concis et plus rapides.

Alternatives : Vous pouvez utiliser `.format()` ou `%` pour l'interpolation, mais les f-strings sont généralement préférées pour leur lisibilité et leur efficacité.

Détails d'implémentation : Les f-strings évaluent au moment de l'exécution les expressions entre `{}` ce qui permet de passer des variables mais aussi d'effectuer des opérations.

```python
# Exemples avec .format() et %
score = 93.5
status = "réussite"
print("Votre score est de {:.1f} et votre statut est : {}".format(score, status))
print("Votre score est de %.1f et votre statut est : %s" % (score, status))
```

Sortie :
```
Votre score est de 93.5 et votre statut est : réussite
Votre score est de 93.5 et votre statut est : réussite
```

## See Also (Voir Aussi)

- [PyFormat](https://pyformat.info/) - Bonnes pratiques sur l'ancienne méthode `.format()`.
- [PEP 498](https://www.python.org/dev/peps/pep-0498/) - Proposition d'amélioration de Python qui a introduit les f-strings.
- [Real Python - f-Strings](https://realpython.com/python-f-strings/) - Guide approfondi sur l'utilisation des f-strings en Python.