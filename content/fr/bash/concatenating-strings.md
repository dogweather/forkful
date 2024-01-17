---
title:                "Concaténation de chaînes"
html_title:           "Bash: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
La concaténation de chaînes ("string concatenation" en anglais) est une opération couramment utilisée en programmation. Elle consiste à fusionner plusieurs chaînes de caractères en une seule. Les programmeurs font cela pour rendre le code plus lisible et pour créer de nouvelles chaînes à partir d'éléments existants.

## Comment faire:
Pour concaténer des chaînes en Bash, vous pouvez utiliser l'opérateur `+` ou simplement les placer côte à côte. Voici un exemple:

Bash
```Bash
nom="Marie"
prenom="Jean"
echo "Bonjour, je m'appelle $prenom $nom."
```
Output
```
Bonjour, je m'appelle Jean Marie.
```

Vous pouvez également utiliser la commande `printf` pour concaténer des chaînes avec un contrôle plus précis sur le formatage. Voici un exemple:

Bash
```Bash
nom="Dupont"
prenom="Paul"
printf "Mon nom est %s et mon prénom est %s." $nom $prenom
```
Output
```
Mon nom est Dupont et mon prénom est Paul.
```

## Plongée en profondeur:
La concaténation de chaînes a été historiquement utilisée en informatique dès les premiers langages de programmation. Elle est également présente dans de nombreux autres langages, tels que Python ou JavaScript. En Bash, vous pouvez également utiliser la commande `echo -n` pour concaténer des chaînes sans saut de ligne à la fin. Enfin, il est important de noter que les chaînes de caractères peuvent être manipulées à l'aide de plusieurs autres opérations telles que la substitution, la recherche et le remplacement, etc.

## Voir aussi:
- [Bash String Manipulation](https://www.shell-tips.com/bash/string-manipulation/)
- [The Art of Bash String Manipulation](https://www.baeldung.com/linux/bash-string-manipulation)
- [Bourne Shell String Manipulation](https://likegeeks.com/linux-shell-scripting-course/string-manipulation/)