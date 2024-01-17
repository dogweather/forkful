---
title:                "Convertir une chaîne en minuscules"
html_title:           "Bash: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi?
La conversion d'une chaîne de caractères en minuscules est le fait de transformer toutes les lettres majuscules d'une chaîne de caractères en minuscules. Les programmeurs font cela pour faciliter la comparaison et la manipulation de chaînes de caractères.

## Comment faire:
Voici quelques exemples de code pour convertir une chaîne de caractères en minuscules en utilisant Bash:

```Bash
# Exemple 1:
my_string="HELLO"
echo "${my_string,,}"  # Output: hello

# Exemple 2:
my_string="Hello World"
tr '[:upper:]' '[:lower:]' <<< "$my_string"  # Output: hello world

# Exemple 3:
my_string="Hello World"
echo "$my_string" | awk '{print tolower($0)}'  # Output: hello world
```

## L'exploration en profondeur:
La conversion d'une chaîne de caractères en minuscules peut sembler une tâche simple, mais cela a une histoire intéressante. Avant 1984, lorsque le standard ASCII était utilisé, il n'existait pas de différence entre lettres majuscules et minuscules. Cependant, avec l'introduction du standard ISO 646, des variations sont apparues comme le code 40 pour les lettres majuscules et le code 60 pour les lettres minuscules. Dans le monde de l'informatique, il existe également d'autres méthodes pour convertir des chaînes de caractères en minuscules, par exemple l'utilisation de la fonction "tr" en utilisant des expressions régulières.

## Voir aussi:
Pour plus d'informations sur la conversion de chaînes de caractères en minuscules, vous pouvez consulter les sources suivantes:

- https://www.gnu.org/software/bash/manual/html_node/Character-Case-Modification.html
- http://tldp.org/LDP/abs/html/string-manipulation.html
- https://www.computerhope.com/unix/bash/tr.htm