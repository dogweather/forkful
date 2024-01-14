---
title:                "Python: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Pourquoi
Les expressions régulières sont un outil puissant pour manipuler et extraire des motifs dans du texte. Elles sont souvent utilisées en programmation pour rechercher, valider et remplacer des chaînes de caractères dans un document ou une base de données. Les expressions régulières peuvent vous faire gagner du temps et vous aider à automatiser des tâches fastidieuses.

# Comment faire
Les expressions régulières sont intégrées à de nombreux langages de programmation, y compris Python. Pour les utiliser, il suffit d'importer le module "re" dans votre code. Voici un exemple de code Python qui utilise des expressions régulières pour trouver toutes les adresses électroniques dans un texte et les afficher :

```
import re

texte = "Mon adresse e-mail est john.doe@exemple.com. N'hésitez pas à me contacter."

adresses = re.findall(r'[\w\.-]+@[\w\.-]+', texte)
print(adresses)
```

Cela affichera `[john.doe@exemple.com]` comme sortie. Expliquons un peu ce qui se passe ici :

- `re.findall()` est la méthode qui utilise l'expression régulière pour trouver toutes les correspondances dans le texte et les renvoie sous forme de liste.
- Le premier argument de `re.findall()` est l'expression régulière elle-même. Dans ce cas, nous utilisons `[w.-]+@[w.-]+` qui correspond à toute chaîne de caractères contenant des lettres, des chiffres, des points ou des tirets avant et après le symbole `@`.
- Le deuxième argument est le texte dans lequel nous recherchons des correspondances. Dans notre exemple, c'est la variable `texte` qui contient notre phrase.

Il existe une multitude d'autres fonctionnalités et règles pour créer des expressions régulières, donc n'hésitez pas à lire la documentation pour en savoir plus.

# Profondeur
Pour être vraiment efficace dans l'utilisation des expressions régulières, il est important de comprendre certains concepts de base tels que les classes de caractères, les quantificateurs, les métacaractères, etc. Vous pouvez également utiliser des outils en ligne tels que RegExr ou Pythex pour tester et expérimenter avec vos expressions régulières avant de les incorporer à votre code.

# Voir aussi
- [Documentation de Python sur les expressions régulières](https://docs.python.org/fr/3/library/re.html)
- [Expressions régulières pour débutants en Python](https://realpython.com/regex-python/#introduction)
- [Outils en ligne pour tester les expressions régulières](https://regex101.com/)