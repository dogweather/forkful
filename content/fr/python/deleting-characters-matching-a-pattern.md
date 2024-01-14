---
title:                "Python: Supprimer les caractères correspondant à un patron"
simple_title:         "Supprimer les caractères correspondant à un patron"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Parfois, lors de la manipulation de chaînes de caractères en Python, vous pouvez avoir besoin de supprimer des caractères spécifiques qui correspondent à un certain schéma. Cela peut sembler un peu intimidant, mais ne vous inquiétez pas, avec quelques connaissances de base, cela peut être facilement réalisé en utilisant des expressions régulières.

## Comment faire

Pour supprimer les caractères correspondant à un pattern en Python, nous allons utiliser le module `re`, qui est dédié au traitement d'expressions régulières. Tout d'abord, nous devons importer le module dans notre code :

```Python
import re
```
Ensuite, nous pouvons utiliser la fonction `sub()`, qui remplace toutes les occurences d'un schéma par une chaîne de caractères vide. Par exemple, si nous voulons supprimer tous les chiffres d'une chaîne, nous pouvons utiliser cette fonction de la manière suivante :

```Python
string = "Hello123World"
new_string = re.sub("[0-9]", "", string) # remplace tous les chiffres par une chaîne vide
print(new_string) # affiche "HelloWorld"
```

Nous pouvons également utiliser des expressions régulières plus complexes pour correspondre à des schémas plus spécifiques. Par exemple, si nous voulons supprimer tous les caractères non alphabétiques d'une chaîne, nous pouvons utiliser cette fonction avec l'expression régulière `[^\w\s]`, qui correspond à tous les caractères non alphabétiques et non blancs.

```Python
string = "Hello, my name is John! How are you?"
new_string = re.sub("[^\w\s]", "", string) # remplace tous les caractères non alphabétiques et non blancs par une chaîne vide
print(new_string) # affiche "Hello my name is John How are you" (tous les signes de ponctuation ont été supprimés)
```

## Plongée en profondeur

L'expression régulière `[^\w\s]` peut sembler un peu complexe, donc voyons de plus près comment cela fonctionne. Tout d'abord, le `^` indique que nous cherchons à correspondre à tout sauf ce qui suit. Ensuite, `\w` correspond à tout caractère alphanumérique, et `\s` correspond à tout caractère blanc (comme les espaces ou les tabulations). Donc, en combinant ces deux éléments avec `^\w\s`, nous pouvons correspondre à tous les caractères non alphabétiques et non blancs.

Si vous souhaitez en savoir plus sur les expressions régulières en Python et tous les modèles que vous pouvez utiliser, je vous recommande d'explorer la documentation de `re` ainsi que des ressources en ligne telles que [les tutoriels de la documentation officielle de Python](https://docs.python.org/fr/3.9/howto/regex.html) et [les exercices pratiques de RegexOne](https://regexone.com/).

## Voir aussi

- [Documentation officielle de Python sur les expressions régulières](https://docs.python.org/fr/3.9/library/re.html)
- [Tutoriel de développement Python sur les expressions régulières](https://docs.python.org/fr/3.9/howto/regex.html)
- [Exercices pratiques de RegexOne](https://regexone.com/)