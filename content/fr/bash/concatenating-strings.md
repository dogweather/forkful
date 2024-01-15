---
title:                "Concaténer des chaînes de caractères"
html_title:           "Bash: Concaténer des chaînes de caractères"
simple_title:         "Concaténer des chaînes de caractères"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

Concaténer des chaînes de caractères peut être très utile en programmation pour combiner du texte de différentes sources et former une nouvelle chaîne de caractères à utiliser dans votre code.

## Comment faire

Voici un exemple de code Bash pour concaténer deux variables de chaîne de caractères :

```Bash
# Définition des variables
nom="Jean"
prenom="Dupont"

# Concaténation des variables pour former une nouvelle chaîne
nom_complet="${nom} ${prenom}"

# Affichage du résultat
echo "Bonjour ${nom_complet}, bienvenue !"
```

Sortie :

```
Bonjour Jean Dupont, bienvenue !
```

Vous pouvez également concaténer des chaînes de caractères directement dans une commande, par exemple :

```Bash
# Définition de variables
fruit="pomme"
adjectif="rouge"

# Utilisation de la concaténation dans une commande
echo "J'adore les ${adjectif}s ${fruit}s."
```

Sortie :

```
J'adore les rouges pommes.
```

## Plongée en profondeur

En Bash, il est possible de concaténer des chaînes de caractères de différentes manières. En plus de l'utilisation de variables, vous pouvez également utiliser des commandes comme `cat` ou `printf` pour combiner du texte. De plus, il est possible de concaténer des chaînes de caractères avec des valeurs numériques en utilisant l'opérateur `+`.

## Voir aussi

- [Documentation officielle de Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Les opérateurs de chaîne dans Bash](https://www.tldp.org/LDP/abs/html/string-manipulation.html)
- [Concaténer des variables en Bash](https://www.techonthenet.com/bash/variables/concat.php)