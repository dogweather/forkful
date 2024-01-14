---
title:                "Python: Suppression de caractères correspondant à un modèle"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi
Supprimer des caractères correspondant à un motif peut être utile lors de la manipulation de chaînes de caractères complexes ou lors du nettoyage de données.

## Comment faire
Voici un exemple de code en Python pour supprimer tous les chiffres d'une chaîne de caractères :

```Python
texte = "Il y a 5 chats dans le jardin"
nouveau_texte = ''.join(c for c in texte if not c.isdigit())
print(nouveau_texte)
```
Le résultat affiché sera : "Il y a chats dans le jardin", les chiffres ayant été supprimés.

Il est également possible d'utiliser des expressions régulières pour supprimer des caractères correspondant à un motif spécifique :

```Python
import re
texte = "Bonjour !<Je suis>un texte<> avec des balises."
nouveau_texte = re.sub('<.*?>', '', texte)
print(nouveau_texte)
```
Le résultat affiché sera : "Bonjour !un texte avec des balises.", les balises ayant été supprimées grâce à l'expression régulière utilisée.

## Plongée en profondeur
L'utilisation de la méthode .join() permet de construire une nouvelle chaîne de caractères en supprimant les éléments qui correspondent au motif spécifié. Cette méthode est très utile lorsque l'on souhaite créer une nouvelle chaîne à partir d'une chaîne existante en supprimant certains éléments.

Pour ce qui est des expressions régulières, il est important de bien comprendre leur structure et leur fonctionnement afin de les utiliser efficacement pour supprimer des caractères correspondant à un motif spécifique.

## Voir aussi
- [Documentation officielle Python - Méthode str.join()](https://docs.python.org/fr/3/library/stdtypes.html#str.join)
- [Documentation officielle Python - Module re](https://docs.python.org/fr/3/library/re.html)