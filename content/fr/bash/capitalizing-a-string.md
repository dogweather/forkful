---
title:                "Majuscule d'une chaîne de caractères"
html_title:           "Bash: Majuscule d'une chaîne de caractères"
simple_title:         "Majuscule d'une chaîne de caractères"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Tu as sûrement déjà eu besoin de capitaliser une chaîne de caractères dans tes scripts Bash. Peut-être pour rendre un texte plus lisible ou pour formater une sortie de commande. Dans cet article, je vais te montrer comment faire cela facilement en utilisant quelques astuces de programmation.

## Comment Faire

La première méthode pour capitaliser une chaîne de caractères consiste à utiliser la commande ```tr```. Voici un exemple de code Bash qui utilise cette méthode :

```Bash
# Déclarer la chaîne de caractères
my_string="ceci est un exemple"

# Utiliser la commande tr pour capitaliser
capitalized_string=$(echo $my_string | tr '[:lower:]' '[:upper:]')

# Afficher la chaîne capitalisée
echo $capitalized_string 

# Sortie : CECI EST UN EXEMPLE
```

Comme tu peux le voir, nous utilisons ici la commande ```echo``` pour envoyer la chaîne de caractères dans le pipeline, puis la commande ```tr``` pour la capitaliser en remplaçant toutes les lettres minuscules par des lettres majuscules.

Une autre méthode consiste à utiliser les paramètres de substitution de Bash. Voici un exemple :

```Bash
# Déclarer la chaîne de caractères
my_string="ceci est un exemple"

# Utiliser les paramètres de substitution pour capitaliser
capitalized_string=${my_string^^}

# Afficher la chaîne capitalisée
echo $capitalized_string 

# Sortie : CECI EST UN EXEMPLE
```

Comme tu peux le voir, cette méthode est plus concise car elle n'utilise qu'une seule ligne de code. Les paramètres de substitution ```^^``` signifient "capitaliser toutes les lettres".

## Deep Dive

Maintenant que tu sais comment capitaliser une chaîne de caractères, tu pourrais te demander comment fonctionne réellement la commande ```tr```. En fait, cette commande utilise le tableau de caractères ASCII pour effectuer le remplacement des lettres. Tu peux consulter ce tableau pour comprendre comment cela fonctionne en détail.

De plus, en utilisant les paramètres de substitution, tu peux également effectuer d'autres opérations sur une chaîne de caractères, comme la convertir en majuscules, en minuscules, ou même inverser l'ordre des lettres.

## Voir Aussi

- [Manuel de la commande tr](https://www.man7.org/linux/man-pages/man1/tr.1.html)
- [Guide de substitution de Bash](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)