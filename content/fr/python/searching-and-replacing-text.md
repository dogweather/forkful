---
title:                "Rechercher et remplacer du texte"
html_title:           "Python: Rechercher et remplacer du texte"
simple_title:         "Rechercher et remplacer du texte"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur ou un utilisateur régulier de Python, vous avez probablement déjà été confronté à la tâche fastidieuse (mais nécessaire) de rechercher et de remplacer du texte dans vos fichiers de code. Heureusement, Python a une fonction intégrée qui facilite cette tâche et permet d'économiser du temps et de l'effort.

## Comment

Pour commencer à utiliser la fonction de recherche et de remplacement de Python, vous aurez besoin d'importer le module `re` dans votre code.

```python
import re
```

Ensuite, vous pouvez utiliser la méthode `sub()` pour rechercher et remplacer du texte dans une chaîne de caractères. Passons en revue un exemple simple:

```python
import re

# Chaîne de caractères avec des fautes de frappe
texte = "Hwllo, je sues un développeur Pythol!"

# Utiliser la fonction sub pour rechercher et remplacer le texte
corrige = re.sub("sues", "suis", texte)

print(corrige)
```

Sortie:

```
Hello, je suis un développeur Python!
```

Comme vous pouvez le voir, en utilisant la fonction `sub`, nous avons facilement corrigé une faute de frappe dans notre chaîne de caractères.

Mais la fonction `sub` peut également être utilisée pour des remplacements plus complexes, en utilisant des expressions régulières. Regardons un autre exemple où nous voulons remplacer tous les nombres dans notre chaîne de caractères par le symbole `#`:

```python
import re

# Chaîne de caractères avec des nombres
texte = "Il y a 5 chats dans le jardin et 3 chiens dans la maison."

# Utiliser la fonction sub avec une expression régulière pour remplacer les nombres par '#'
modifie = re.sub("\d+", "#", texte)

print(modifie)
```

Sortie:

```
Il y a # chats dans le jardin et # chiens dans la maison.
```

Cet exemple montre comment la fonction `sub` peut être utilisée pour effectuer des recherches et des remplacements complexes en utilisant des expressions régulières.

## Deep Dive

Pour une compréhension plus approfondie de la fonction `sub` de Python, il est utile de savoir que la méthode peut prendre un troisième argument optionnel, `count`, qui spécifie le nombre de fois que la fonction doit effectuer le remplacement. Par défaut, `count` est réglé sur 0, ce qui signifie qu'il effectuera un remplacement sur toutes les occurrences trouvées. Vous pouvez également spécifier un nombre dans `count` pour limiter le nombre de remplacements effectués.

De plus, la méthode `sub` peut également prendre en compte les options de filtrage et de traitement des caractères. Vous pouvez en savoir plus sur ces options et leur utilisation dans la documentation officielle de Python sur la fonction `sub`.

## Voir aussi

Consultez ces liens pour en savoir plus sur les expressions régulières en Python et leur utilisation dans la fonction `sub`:

- [Documentation officielle Python sur `re.sub`](https://docs.python.org/3/library/re.html#re.sub)
- [Guide complet sur les expressions régulières en Python](https://www.datacamp.com/community/tutorials/python-regular-expression-tutorial)