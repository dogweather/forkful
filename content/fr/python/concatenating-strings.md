---
title:    "Python: Concaténation de chaînes de caractères"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

La concaténation de chaînes de caractères est un outil essentiel pour tout développeur Python. Cela permet de combiner plusieurs chaînes de caractères en une seule, ce qui est utile pour la création de messages, de noms de fichiers ou bien encore de requêtes pour une API.

## Comment faire

Pour concaténer des chaînes de caractères en Python, vous pouvez utiliser l'opérateur de concaténation '+'. Par exemple :

```Python
prenom = "Jean"
nom = "Dupont"
age = 25

nom_complet = prenom + " " + nom
print(nom_complet) # Output : Jean Dupont

message = "Je m'appelle " + nom_complet + " et j'ai " + str(age) + " ans."
print(message) # Output : Je m'appelle Jean Dupont et j'ai 25 ans.
```

Comme vous pouvez le voir, nous avons utilisé l'opérateur '+' pour concaténer des chaînes de caractères et même une variable entière (age) en utilisant la fonction str() pour la convertir en chaîne.

## Plongée en profondeur

Il est important de noter que la concaténation peut également être réalisée avec la méthode .format(), qui est une méthode plus flexible pour insérer des variables dans une chaîne de caractères. Par exemple :

```Python
prenom = "Marie"
nom = "Martin"
age = 20

message = "Je m'appelle {nom} {prenom} et j'ai {age} ans.".format(nom=nom, prenom=prenom, age=age)
print(message) # Output : Je m'appelle Martin Marie et j'ai 20 ans.
```

De plus, il est également possible de concaténer des chaînes de caractères en utilisant des f-strings (ou formatted string literals) introduites en Python 3.6. Avec cette méthode, les variables sont insérées directement dans la chaîne en utilisant les accolades et en préfixant la chaîne avec la lettre 'f'. Reprenons l'exemple précédent :

```Python
prenom = "Marie"
nom = "Martin"
age = 20

message = f"Je m'appelle {nom} {prenom} et j'ai {age} ans."
print(message) # Output : Je m'appelle Martin Marie et j'ai 20 ans.
```

Les f-strings offrent une syntaxe plus concise et plus lisible pour la concaténation de chaînes de caractères.

## Voir aussi

- [Python String Formatting: How to Use .format()](https://www.digitalocean.com/community/tutorials/how-to-use-string-formatters-in-python-3)
- [Python String Concatenation: Tips and Tricks](https://realpython.com/python-string-formatting/)
- [PEP 498 -- Formatted string literals](https://www.python.org/dev/peps/pep-0498/)