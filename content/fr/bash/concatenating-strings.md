---
title:                "Concaténation de chaînes"
html_title:           "C: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?

La concaténation de chaînes consiste à joindre deux chaînes ou plus pour former une seule chaîne. Les programmeurs l'utilisent pour combiner des informations et générer des sorties dynamiques.

## Comment faire:

Il est assez simple de concaténer des chaînes en Bash. Voici quelques exemples :

```Bash
# Déclaration de variables
string1="Bonjour,"
string2=" comment ça va?"

# Concaténation
greetings=$string1$string2
echo $greetings
```

Sortie :
```Bash
Bonjour, comment ça va?
```

## Plongée plus profonde:

Bash, sorti en 1989, permet la concaténation de chaînes depuis ses débuts, faisant partie de sa syntaxe de base. Toutefois, il existe d'autres alternatives dans d'autres langages de programmation, par exemple `+` dans Python ou `.concat()` en JavaScript. La concaténation de chaînes en Bash est simple : lors de l'affectation, Bash ne rajoute pas d'espace entre les variables. C'est donc à vous de gérer ces espaces manuellement.

## Voir aussi:

Pour en savoir plus sur la concaténation de chaînes en Bash, consultez ces ressources : 

- [GNU Bash Reference Manual](https://www.gnu.org/software/bash/manual/bash.html)
- [Bash String Manipulation Guide](https://www.tldp.org/LDP/abs/html/string-manipulation.html)
- [Bash Guide for Beginners](https://www.tldp.org/LDP/Bash-Beginners-Guide/html/index.html)