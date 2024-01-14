---
title:                "Python: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/extracting-substrings.md"
---

{{< edit_this_page >}}

# Pourquoi

L'extraction de sous-chaînes de caractères est une compétence importante en programmation Python. Elle vous permet de manipuler et d'utiliser efficacement des chaines de caractères dans vos projets. Que ce soit pour le traitement de fichiers, la génération de rapports ou le traitement de données, la capacité à extraire des sous-chaînes vous sera utile dans de nombreuses situations.

# Comment faire

Pour extraire une sous-chaîne de caractères dans Python, vous pouvez utiliser la notation de tranche (slicing notation) ou la méthode `split()`. Voici quelques exemples pour vous montrer comment cela fonctionne:

```Python
# Utilisation de la notation de tranche
chaine = "Bonjour les amis"
print(chaine[3:7])
# Output: jour

# Utilisation de la méthode split()
chaine = "Bonjour les amis"
print(chaine.split(" "))
# Output: ['Bonjour', 'les', 'amis']
```
Dans le premier exemple, nous avons utilisé la notation de tranche pour extraire la sous-chaîne "jour". La syntaxe est `chaine[debut:fin]` où `debut` et `fin` sont des indices représentant les positions de début et de fin de la sous-chaîne souhaitée. Gardez à l'esprit que la tranche s'arrête à l'indice `fin-1`.

Dans le deuxième exemple, nous avons utilisé la méthode `split()` pour diviser la chaine en une liste de sous-chaînes en utilisant l'espace comme séparateur. Vous pouvez également spécifier un autre séparateur si nécessaire.

# Plongée en profondeur

Il est important de comprendre que les indices dans Python commencent à zéro, ce qui signifie que le premier caractère d'une chaîne a l'indice 0. De plus, si vous n'indiquez pas de valeur pour `debut` ou `fin`, Python utilisera respectivement 0 et la longueur de la chaîne. Ainsi, `chaine[:4]` extraira les 4 premiers caractères de la chaine et `chaine[4:]` extraira tous les caractères à partir du 5ème.

De plus, la notation de tranche vous permet également de spécifier un pas, ce qui vous permet de sauter certains caractères lors de l'extraction de la sous-chaîne. Par exemple, `chaine[1:10:2]` extraira les caractères aux indices 1, 3, 5, 7 et 9.

Enfin, la méthode `split()` peut prendre un argument optionnel `maxsplit` pour limiter le nombre de sous-chaînes extraites. Par exemple, `chaine.split(" ", 1)` renverra seulement deux sous-chaînes, la première étant "Bonjour" et la deuxième "les amis".

# Voir aussi

- [Documentation Python sur la notation de tranche](https://docs.python.org/fr/3/glossary.html#term-slicing)
- [Documentation Python sur la méthode split()](https://docs.python.org/fr/3/library/stdtypes.html#str.split)