---
title:                "Extraction de sous-chaines"
html_title:           "Python: Extraction de sous-chaines"
simple_title:         "Extraction de sous-chaines"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/extracting-substrings.md"
---

{{< edit_this_page >}}

##Qu'est-ce que c'est et pourquoi le faire?

L'extraction de sous-chaînes, également connue sous le nom de découpage de chaîne, est une méthode couramment utilisée par les programmeurs pour extraire une partie spécifique d'une chaîne de caractères. Cela peut être utile lors du traitement de données ou de la manipulation de chaînes de texte pour des tâches spécifiques.

##Comment faire:

Utilisez la méthode de découpage de chaîne ```Python ma_chaine[slice]``` pour extraire la partie souhaitée de votre chaîne. Par exemple:

```
ma_chaine = "Bonjour tout le monde"
print(ma_chaine[0:6])
```

Cette ligne de code imprimera "Bonjour", car nous utilisons une tranche pour extraire les six premiers caractères de la chaîne.

##Plongée en profondeur:

L'extraction de sous-chaînes existe depuis les premiers langages de programmation et est toujours largement utilisée aujourd'hui. Cependant, il existe également des alternatives telles que l'utilisation de méthodes telles que ```split()``` pour séparer une chaîne en plusieurs parties ou l'utilisation de regex pour des cas plus complexes.

Il est également important de noter que l'extraction de sous-chaînes pourrait être réalisée en utilisant des indices négatifs, en partant de la fin de la chaîne au lieu du début.

##Voir aussi:

- [Documentation officielle Python pour la méthode de découpage de chaîne](https://docs.python.org/fr/3/library/stdtypes.html#str.slice)
- [Documentation officielle Python pour la méthode split()](https://docs.python.org/fr/3/library/stdtypes.html#str.split)
- [Documentation officielle Python pour les expressions régulières](https://docs.python.org/fr/3/library/re.html)