---
title:                "Convertir une chaîne en minuscules"
html_title:           "Ruby: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Il peut être utile de convertir une chaîne de caractères en minuscules pour faciliter la comparaison de chaînes, notamment lorsque l'on travaille avec du texte saisie par des utilisateurs. Cela permet également de rendre le texte plus cohérent en le convertissant en un seul casse.

## Comment faire

Pour convertir une chaîne en minuscules en Ruby, il suffit d'utiliser la méthode `downcase` comme suit :

```Ruby
string = "Ceci est un exemple de STRING"
puts string.downcase
```

Cela produira la sortie suivante :

```
ceci est un exemple de string
```

Il est également possible d'utiliser une assignation de variable pour convertir directement la chaîne en minuscules, comme ceci :

```Ruby
string = "Ceci est un autre exemple de STRING"
uppercase_string = string.downcase
puts uppercase_string
```

## Plongée en profondeur

Lorsque vous appelez la méthode `downcase` sur une chaîne de caractères, Ruby utilise la table ASCII pour effectuer la conversion. Cela signifie que seules les lettres majuscules de l'alphabet anglais seront converties en minuscules. Les lettres accentuées, les caractères spéciaux et les chiffres ne seront pas affectés.

Il est également important de noter que la méthode `downcase` ne modifie pas la chaîne d'origine, elle renvoie plutôt une nouvelle chaîne avec les modifications. La chaîne d'origine reste donc inchangée. Si vous souhaitez modifier directement la chaîne, vous pouvez utiliser `downcase!` avec un point d'exclamation à la fin.

## Voir aussi

- [Documentation de la méthode downcase en Ruby](https://ruby-doc.org/core-2.7.1/String.html#method-i-downcase)
- [Table ASCII](https://fr.wikipedia.org/wiki/American_Standard_Code_for_Information_Interchange)