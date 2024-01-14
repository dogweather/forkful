---
title:                "Bash: Concaténation de chaînes de caractères"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

Concaténer des chaînes de caractères est une compétence importante en programmation Bash. Cela permet de combiner plusieurs chaînes de caractères pour créer de nouveaux messages ou des noms de fichier uniques. Cela peut également être utile pour formater des données avant de les afficher. 

## Comment Faire

Pour concaténer des chaînes de caractères en Bash, nous utilisons l'opérateur de concaténation "+". Voici un exemple de code qui combine deux chaînes de caractères et les imprime à l'écran :

```Bash
nom="John"
prenom="Doe"
echo "Bonjour, je m'appelle "$nom" "$prenom"."
```

Lorsque ce code est exécuté, il affichera "Bonjour, je m'appelle John Doe." Notez comment nous avons utilisé la variable "$" pour appeler le contenu de chaque chaîne de caractères.

Nous pouvons également utiliser l'opérateur de concaténation "+" avec des variables pour créer des chaînes de caractères plus complexes. Par exemple :

```Bash
nom="John"
prenom="Doe"
age=30
echo "Bonjour, je m'appelle "$nom" "$prenom" et j'ai "$age" ans."
```

Ce code affichera "Bonjour, je m'appelle John Doe et j'ai 30 ans."

## Plongée en Profondeur

En plus de l'opérateur de concaténation "+", nous pouvons également utiliser la fonction "concat" en Bash pour combiner plusieurs chaînes de caractères. Cette fonction prend en paramètre toutes les chaînes à concaténer et renvoie le résultat sous forme de nouvelle chaîne de caractères.

Il est également possible de concaténer des variables avec des chaînes de caractères directement dans une commande. Par exemple :

```Bash
prenom="John"
echo "Bonjour "$prenom", bienvenue sur mon site."
```

Cela affichera "Bonjour John, bienvenue sur mon site."

## Voir Aussi

Pour plus d'informations sur la concaténation en Bash, vous pouvez consulter ces liens supplémentaires :

- [Documentation officielle de Bash sur la concaténation](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
- [Vidéo explicative sur la concatenation en Bash (en anglais)](https://www.youtube.com/watch?v=IU08gtXcvKg)
- [Article sur l'utilisation des variables en Bash](https://lionfacelemonface.com/blog/using-variables-in-bash-scripts/)