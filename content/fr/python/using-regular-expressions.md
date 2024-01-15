---
title:                "Utiliser les expressions régulières"
html_title:           "Python: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi utiliser les expressions régulières en Python?

Les expressions régulières sont des outils puissants pour la manipulation de chaînes de caractères. Elles permettent de rechercher, de modifier et de valider des motifs dans du texte. Utilisées en Python, elles peuvent simplifier grandement la gestion de données et les opérations sur des chaînes.

## Comment utiliser les expressions régulières en Python

Pour utiliser les expressions régulières en Python, il faut d'abord importer le module `re`:

```Python
import re
```

Ensuite, il faut créer un objet de type `pattern` en utilisant la fonction `compile()`:

```Python
pattern = re.compile(r'motif')
```

Le motif doit être précédé du caractère `r` pour indiquer qu'il s'agit d'une chaîne brute. Ensuite, on peut utiliser les méthodes `search()` ou `match()` pour rechercher le motif dans une chaîne:

```Python
result = pattern.search('ceci est un exemple de motif')
```

Le résultat sera un objet `match` qui pourra être utilisé pour extraire ou modifier le motif trouvé. Par exemple, on peut remplacer le motif par un autre texte avec la méthode `sub()`:

```Python
new_text = pattern.sub('nouveau motif', 'ceci est un exemple de motif')
```

## Plongée en profondeur: conseils et astuces pour bien utiliser les expressions régulières en Python

- Les symboles `^` et `$` permettent de délimiter le début et la fin d'une chaîne respectivement. Par exemple, `^motif` recherchera le motif uniquement s'il se trouve au début de la chaîne.
- Les crochets `[ ]` permettent de définir un ensemble de caractères à rechercher. Par exemple, `[aeiou]` cherchera n'importe lequel de ces voyelles dans une chaîne.
- L'utilisation de parenthèses `()` permet de capturer des groupes de caractères pour les réutiliser dans la substitution avec la méthode `sub()`.
- Le symbole `?` permet de rendre un caractère facultatif. Par exemple, `colou?r` cherchera à la fois «color» et «colour».

## Voir aussi

- Documentation officielle de Python sur les expressions régulières: https://docs.python.org/fr/3/library/re.html
- Tutoriel interactif sur les expressions régulières en Python: https://regexone.com/references/python