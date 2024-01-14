---
title:    "Python: Extraction de sous-chaînes"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Pourquoi

L'extraction de sous-chaînes (ou sous-strings en anglais) est une compétence essentielle en programmation Python. Cela permet aux développeurs de découper une chaîne de caractères en de plus petites parties, ce qui peut être utile dans de nombreuses situations, notamment dans la manipulation de données ou la création de fonctions réutilisables.

## Comment faire

Pour extraire une sous-chaîne en Python, il suffit d'utiliser la notation de tranche (slice en anglais). Voici un exemple de code qui illustre comment extraire une sous-chaîne à partir d'une chaîne de caractères :

```python
chaine = "Bonjour tout le monde"
sous_chaine = chaine[8:13]
print(sous_chaine)
```

Dans cet exemple, nous avons utilisé la notation de tranche pour extraire la sous-chaîne "tout" à partir de la chaîne d'origine. La notation de tranche fonctionne comme ceci : `chaine[début:fin]`, où `début` est l'indice du premier caractère de la sous-chaîne et `fin` est l'indice du dernier caractère + 1.

La notation de tranche peut également prendre un troisième paramètre facultatif, qui représente le pas (step en anglais). Cela permet d'extraire des caractères en sautant un certain nombre de caractères entre chaque indice. Voici un exemple qui illustre cette fonctionnalité :

```python
chaine = "Exemple de chaîne"
sous_chaine = chaine[0:13:2]
print(sous_chaine)
```

Dans cet exemple, le résultat serait "Eexml d", car nous avons sauté un caractère sur deux à chaque tranche.

## Plongée en profondeur

En plus de la notation de tranche, Python offre d'autres méthodes pour extraire des sous-chaînes, telles que la méthode `split()` qui permet de séparer une chaîne en plusieurs morceaux en utilisant un séparateur spécifié, ou la méthode `find()` qui renvoie l'indice de la première occurrence d'une sous-chaîne donnée.

Il est également important de prendre en compte la manipulation des indices lorsque l'on extrait des sous-chaînes en Python, car les indices commencent à partir de 0 et le premier indice est inclus tandis que le dernier indice n'est pas inclus. Il est donc essentiel de bien comprendre comment fonctionne la notation de tranche afin d'éviter des erreurs dans votre code.

## Voir aussi

Voici quelques liens utiles pour en savoir plus sur l'extraction de sous-chaînes en Python :

- [Documentation officielle Python pour la notation de tranche](https://docs.python.org/fr/3/library/stdtypes.html#textseq)
- [Article de Real Python sur l'extraction de sous-chaînes](https://realpython.com/python-strings/#slicing-strings)
- [Vidéo YouTube sur l'utilisation de la notation de tranche en Python](https://www.youtube.com/watch?v=ajrtAuDg3yw)