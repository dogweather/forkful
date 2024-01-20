---
title:                "Interpolation d'une chaîne de caractères"
html_title:           "Ruby: Interpolation d'une chaîne de caractères"
simple_title:         "Interpolation d'une chaîne de caractères"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce et pourquoi?

La **l'interpolation de chaîne** est la procédure par laquelle on insère des variables dans une chaîne de caractères. Les programmeurs l'utilisent pour rendre le code plus concis et plus facile à lire.

## Comment faire:

Voici un exemple simple d'interpolation de chaîne en Bash:

```Bash
#!/bin/bash
nom="Paul"
echo "Bonjour, $nom"
```

Cela donnerait la sortie suivante:

```Bash
Bonjour, Paul
```

On peut aussi utiliser des accolades pour définir clairement la variable. C'est particulièrement utile lorsqu'on concatène des variables avec du texte.

```Bash
#!/bin/bash
nom="Paul"
echo "Bonjour, ${nom}, comment allez-vous aujourd'hui?"
```

## Plongeons un peu plus profondément:

Historiquement, Bash a introduit l'interpolation de chaîne pour faciliter la composition de commandes complexes. Il existe d'autres alternatives, comme l'utilisation de la concaténation de chaînes, mais l'interpolation est en général plus concise et plus claire.

Il convient de noter que, contrairement à d'autres langages de script tels que Perl ou Ruby, Bash ne soutient pas directement l'interpolation de chaînes complexes (c'est-à-dire, une expression au sein de la chaîne). Néanmoins, vous pouvez obtenir un comportement similaire en utilisant des commandes intégrées comme 'eval' ou 'printf'.

## Voir aussi:

Pour en savoir plus sur l'interpolation de chaîne en Bash, consultez ces ressources:

- [Bash String](https://www.linuxtopia.org/online_books/bash_guide_for_beginners/sect_03_03.html): Un guide pour débutants sur le manipulation des chaînes de caractères en Bash.
- [Bash Guide](https://tldp.org/LDP/abs/html/string-manipulation.html): Un guide complet sur l'interpolation de chaînes et la manipulation de chaînes en Bash.
- [Bash Variables](http://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO-5.html): Un guide sur l'utilisation des variables dans Bash.