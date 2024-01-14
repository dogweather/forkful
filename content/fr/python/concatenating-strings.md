---
title:    "Python: Concaténation de chaînes de caractères"
keywords: ["Python"]
---

{{< edit_this_page >}}

# Pourquoi

Concaténer des chaînes de caractères est une tâche courante en programmation, notamment en Python. Cela permet de combiner plusieurs chaînes de caractères en une seule, ce qui peut être utile pour créer des messages personnalisés ou pour formater des données.

# Comment faire

Pour concaténer des chaînes de caractères en Python, nous utilisons l'opérateur "+". Par exemple :

```Python
prenom = "Marie"
nom = "Dupont"
message = "Bienvenue " + prenom + " " + nom + " !"
print(message)
```
Résultat :

```
Bienvenue Marie Dupont !
```

Nous pouvons également utiliser la méthode `format()` pour insérer des variables dans une chaîne de caractères. Par exemple :

```Python
prenom = "Marie"
nom = "Dupont"
message = "Bienvenue {} {} !".format(prenom, nom)
print(message)
```
Résultat :

```
Bienvenue Marie Dupont !
```

Il est également possible de concaténer des chaînes de caractères avec des chiffres en utilisant la fonction `str()` pour les convertir en chaînes. Par exemple :

```Python
nombre = 8
phrase = "J'aime les chiffres " + str(nombre)
print(phrase)
```
Résultat :

```
J'aime les chiffres 8
```

# Plongée en profondeur

En Python, les chaînes de caractères sont immuables, ce qui signifie qu'elles ne peuvent pas être modifiées directement. Lors de l'utilisation de l'opérateur "+", une nouvelle chaîne de caractères est créée pour contenir la concaténation des deux chaînes d'origine. Cela peut entraîner des problèmes de performance si vous concaténez plusieurs chaînes à chaque itération d'une boucle. Dans ces cas, il est préférable d'utiliser la méthode `join()` ou de formater une seule chaîne avec plusieurs variables.

# Voir aussi

- [Documentation officielle de Python sur les chaînes de caractères](https://docs.python.org/fr/3/library/stdtypes.html#text-sequence-type-str)
- [Tutorial sur les chaînes de caractères en Python](https://www.w3schools.com/python/python_strings.asp)
- [Manipuler des chaînes de caractères en Python](https://realpython.com/python-strings/)