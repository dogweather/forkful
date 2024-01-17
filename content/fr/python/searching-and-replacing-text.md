---
title:                "Recherche et remplacement de texte"
html_title:           "Python: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & pourquoi ?
Remplacer du texte est une tâche courante pour les programmeurs. Cela consiste à trouver un certain motif ou une chaîne de caractères dans un texte et à la remplacer par une autre chaîne de caractères. Les programmeurs le font pour automatiser les changements de texte dans leurs programmes et pour gagner du temps lors de la mise à jour de leur code.

## Comment faire :
Pour remplacer du texte en Python, il existe différentes méthodes dont la plus utilisée est la fonction `replace()`. Elle prend deux arguments, le motif ou la chaîne de caractères à remplacer et la nouvelle chaîne de caractères. Voici un exemple de code :

```Python
my_string = "Hello World!"
new_string = my_string.replace("World", "Universe")
print(new_string)
```

Résultat :
```
Hello Universe!
```

Il existe également la méthode `sub()` du module `re` qui permet de remplacer du texte en utilisant des expressions régulières. Voici un exemple de code :

```Python
import re

my_string = "Hello 123!"
new_string = re.sub(r"[0-9]+", "World", my_string)
print(new_string)
```

Résultat :
```
Hello World!
```

## Deep Dive :
De nombreux langages de programmation offrent des moyens de rechercher et de remplacer du texte, mais certains sont plus performants que d'autres. Par exemple, l'utilisation d'expressions régulières en Python peut être plus complexe que dans d'autres langages, mais elle offre une grande flexibilité.

Pour les programmeurs débutants ou ceux qui souhaitent simplement effectuer des remplacements de base, la fonction `replace()` est généralement suffisante. Cependant, pour des besoins plus avancés, il peut être utile d'explorer les possibilités offertes par les expressions régulières.

## Voir aussi :
- La documentation officielle de Python sur la fonction `replace()` : https://docs.python.org/3/library/stdtypes.html#str.replace
- La documentation officielle de Python sur le module `re` : https://docs.python.org/3/library/re.html
- Un tutoriel sur les expressions régulières en Python : https://www.w3schools.com/python/python_regex.asp