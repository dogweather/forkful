---
title:                "Python: Concaténation de chaînes"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

La concaténation de chaînes est une pratique courante en programmation, utilisée pour combiner plusieurs chaînes de caractères en une seule. Cela peut être utile pour créer des messages d'erreur personnalisables, des noms de fichiers dynamiques ou simplement pour afficher du texte formaté. Dans cet article, nous allons explorer comment concaténer des chaînes en Python et pourquoi cela peut être utile pour vos projets de programmation.

## Comment faire

Pour concaténer des chaînes en Python, nous pouvons utiliser l'opérateur "+" ou la méthode .format(). Voyons un exemple de chaque dans un bloc de code :

```Python
# Utiliser l'opérateur "+"
nom = "Marie"
message = "Bienvenue sur notre site, " + nom + "!"
print(message)

# Utiliser la méthode .format()
prenom = "Jean"
nom = "Dupont"
message = "Bonjour {0} {1}, comment allez-vous ?".format(prenom, nom)
print(message)
```

La sortie de ces deux exemples sera :

```
Bienvenue sur notre site, Marie!
Bonjour Jean Dupont, comment allez-vous ?
```

Comme vous pouvez le voir, l'opérateur "+" nous permet de simplement combiner les chaînes en les plaçant entre les variables, tandis que la méthode .format() utilise des espaces réservés spéciaux pour insérer les variables dans le texte. Dans les deux cas, nous obtenons une chaîne concaténée qui peut être affichée avec print().

## Plongée plus profonde

En plus de l'opérateur "+" et de la méthode .format(), il existe d'autres moyens de concaténer des chaînes en Python. En utilisant l'opérateur "+=", nous pouvons ajouter une chaîne à elle-même pour la prolonger, comme ceci :

```Python
message = "Bonjour!"
message += " Comment ça va ?"
print(message)
```

La sortie sera :

```
Bonjour! Comment ça va ?
```

Nous pouvons également utiliser la méthode .join() pour concaténer une liste de chaînes en une seule. Voici un exemple :

```Python
prenoms = ["Jean", "Marie", "Pierre"]
noms = ["Dupont", "Martin", "Lefevre"]

message = "Bienvenue à {0}, {1}".join(noms, prenoms)
print(message)
```

La sortie sera :

```
Bienvenue à Dupont, Martin, Lefevre, Jean, Marie, Pierre
```

Il est également important de noter qu'en Python, les chaînes sont immuables, ce qui signifie qu'elles ne peuvent pas être modifiées directement. Cela signifie que chaque fois que nous concaténons une chaîne, nous créons en fait une nouvelle chaîne avec les modifications requises.

## Voir aussi

- [Documentation Python sur les chaînes de caractères](https://docs.python.org/fr/3/tutorial/introduction.html#strings)
- [Cours sur les chaînes de caractères en français](https://openclassrooms.com/fr/courses/235344-apprenez-a-programmer-en-python/234814-manipulez-des-chaines-de-caracteres)

En utilisant les différentes méthodes de concaténation de chaînes en Python, vous pourrez facilement créer des messages et des noms de fichiers dynamiques, et vous pourrez également ajouter facilement du texte formaté à vos projets de programmation. N'hésitez pas à expérimenter et à trouver la méthode qui fonctionne le mieux pour vous !