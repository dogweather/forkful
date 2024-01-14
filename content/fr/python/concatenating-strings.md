---
title:                "Python: Concaténer des chaînes"
simple_title:         "Concaténer des chaînes"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/concatenating-strings.md"
---

{{< edit_this_page >}}

# Pourquoi

Concaténer des chaînes de caractères est une compétence fondamentale en programmation Python. Cela permet de combiner plusieurs chaînes en une seule, ce qui est utile pour afficher des messages personnalisés, créer des noms de fichiers dynamiques, ou encore construire des requêtes de base de données.

# Comment faire

En Python, il existe plusieurs façons de concaténer des chaînes de caractères. La première méthode est d'utiliser l'opérateur "+" pour combiner deux chaînes :

```Python
nom = "Sophie"
message = "Bonjour, je m'appelle " + nom
print(message)
```
Output:
```
Bonjour, je m'appelle Sophie
```

Une autre méthode consiste à utiliser la méthode `format()` pour remplacer des variables dans une chaîne :

```Python
prenom = "Jean"
nom = "Dupont"
age = 30
message = "Bonjour, je m'appelle {} {} et j'ai {} ans".format(prenom, nom, age)
print(message)
```
Output:
```
Bonjour, je m'appelle Jean Dupont et j'ai 30 ans
```

Enfin, la méthode la plus récente est l'utilisation de f-strings (string literals formatées) qui permet d'écrire du code plus lisible :

```Python
prenom = "Marie"
nom = "Martin"
age = 25
message = f"Bonjour, je m'appelle {prenom} {nom} et j'ai {age} ans"
print(message)
```
Output:
```
Bonjour, je m'appelle Marie Martin et j'ai 25 ans
```

# Plongée profonde

Lorsqu'on concatène des chaînes de caractères, il est important de faire attention aux types de données utilisés. Par exemple, si l'un des éléments à concaténer est un entier, il faut le convertir en chaîne en utilisant la fonction `str()` :

```Python
age = 25
message = "J'ai " + str(age) + " ans"
print(message)
```
Output:
```
J'ai 25 ans
```

Il est également possible de concaténer plus de deux chaînes en une seule fois :

```Python
prenom = "Paul"
nom = "Dupuis"
ville = "Paris"
message = "Je m'appelle " + prenom + " " + nom + " et j'habite à " + ville
print(message)
```
Output:
```
Je m'appelle Paul Dupuis et j'habite à Paris
```

# Voir aussi

- [Python String Concatenation](https://www.programiz.com/python-programming/string-concatenation)
- [Python Documentations sur les f-strings](https://docs.python.org/fr/3/library/string.html#formatstrings)