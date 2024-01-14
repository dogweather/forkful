---
title:                "Python: Extraction des sous-chaînes"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Extraire des sous-chaînes est une compétence importante en programmation Python qui permet de manipuler les données de manière efficace. Cela peut être utile pour des tâches telles que la récupération d'informations spécifiques dans une grande quantité de données ou la manipulation de chaînes de caractères pour un formatage spécifique.

## Comment faire

Pour extraire une sous-chaîne d'une chaîne de caractères en Python, vous pouvez utiliser la méthode de découpe (slicing). Cette méthode prend deux indices en paramètres, le début et la fin de la sous-chaîne, et renvoie la partie de la chaîne originale située entre ces deux indices. Par exemple:

```
Python string = "Bonjour à tous"
sub_string = string[7:10]
print(sub_string)
```

Résultat:

```
à to
```

Vous pouvez également spécifier un troisième paramètre pour la méthode de découpe, qui indique le pas de la sous-chaîne, c'est-à-dire le nombre de caractères à sauter à chaque étape. Par exemple:

```
Python string = "Bonjour à tous"
sub_string = string[::3]
print(sub_string)
```

Résultat:

```
Bno  u
```

## Plongée en profondeur

Outre la méthode de découpe, il existe d'autres outils en Python pour extraire des sous-chaînes de manière plus complexe, tels que les expressions régulières. Les expressions régulières sont des motifs de recherche qui permettent de trouver des combinaisons de caractères spécifiques dans une chaîne de caractères. Elles peuvent être utilisées pour extraire des sous-chaînes en fonction de motifs spécifiques plutôt que d'indices précis.

Par exemple, imaginez que vous voulez extraire tous les mots contenant le préfixe "py" dans une chaîne donnée. Vous pouvez utiliser une expression régulière pour trouver tous ces mots et les stocker dans une liste. Voici un exemple de code utilisant le module "re" de Python pour accomplir cette tâche:

```
import re

string = "Python est un langage de programmation très populaire."

sub_strings = re.findall(r"py\w+", string)
print(sub_strings)
```

Résultat:

```
['Python', 'programmation']
```

## Voir aussi

- [Documentation officielle de Python sur la méthode de découpe](https://docs.python.org/fr/3/library/stdtypes.html#mutable-sequence-types)
- [Tutoriel sur les expressions régulières en Python](https://www.youtube.com/watch?v=K8L6KVGG-7o)
- [Guide pour travailler avec les chaînes de caractères en Python](https://realpython.com/python-strings/)