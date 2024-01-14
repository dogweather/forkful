---
title:    "Python: Recherche et remplacement de texte"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

La recherche et le remplacement de texte sont des outils extrêmement utiles pour tout programmeur Python. Avec ces outils, vous pouvez facilement modifier de grandes quantités de texte dans vos scripts, ce qui peut vous faire gagner un temps précieux lors du développement.

## Comment faire

La recherche et le remplacement de texte sont réalisables en utilisant la méthode ```.replace()```. Cette méthode prend deux paramètres: le texte à rechercher et le texte de remplacement. Par exemple, si vous voulez remplacer tous les espaces par des tirets dans une chaîne de caractères, vous pouvez utiliser la ligne de code suivante:

```
texte_modifie = texte_original.replace(" ", "-")
```

Dans cet exemple, nous avons défini la variable ```texte_original``` contenant notre texte et nous avons utilisé la méthode ```.replace()``` pour remplacer chaque espace par un tiret. Le nouveau texte modifié sera stocké dans la variable ```texte_modifie```.

En plus de remplacer, vous pouvez également utiliser la méthode ```.find()``` pour rechercher un texte spécifique dans une chaîne de caractères. Cette méthode renverra l'index du premier caractère du texte recherché. Par exemple, si vous voulez trouver l'index du premier "e" dans une chaîne de caractères, vous pouvez utiliser la ligne de code suivante:

```
index = texte_original.find("e")
```

Si le texte recherché n'est pas trouvé, la méthode renverra -1.

## Plongez plus profondément

Il existe plusieurs paramètres optionnels que vous pouvez ajouter à la méthode ```.replace()``` pour personnaliser votre recherche et remplacement. Par exemple, vous pouvez spécifier le nombre maximum de remplacements à effectuer en ajoutant un troisième paramètre à la méthode. Vous pouvez également utiliser la méthode avec des chaînes de caractères plus complexes, telles que des expressions régulières, pour des remplacements plus avancés.

De plus, il est important de noter que la méthode ```.replace()``` ne modifiera pas directement la chaîne de caractères d'origine. Elle renverra une nouvelle chaîne de caractères modifiée, qui peut être ensuite assignée à une nouvelle variable ou être utilisée directement.

## Voir aussi

- [Documentation officielle de Python sur la méthode .replace()](https://docs.python.org/fr/3/library/stdtypes.html#str.replace)
- [Autres méthodes de recherche et de remplacement de texte en Python](https://www.tutorialspoint.com/python/string_replace.htm)
- [Un guide complet sur les expressions régulières en Python](https://realpython.com/regex-python/)