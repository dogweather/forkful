---
title:                "Fish Shell: Capitaliser une chaîne de caractères"
simple_title:         "Capitaliser une chaîne de caractères"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi 
Si vous utilisez le shell de poisson (Fish Shell), vous pourriez vous demander pourquoi il serait utile de capitaliser une chaîne de caractères. La réponse est simple : cela peut être très pratique lorsque vous travaillez avec des données saisies par des utilisateurs, des fichiers texte ou tout autre contenu nécessitant une casse cohérente.

## Comment faire 
Pour capitaliser une chaîne de caractères en utilisant Fish Shell, vous pouvez suivre ces étapes simples :

- Utilisez la commande `string toupper` pour convertir tous les caractères de la chaîne en majuscules.
- Utilisez la commande `string substring` pour récupérer la première lettre de la chaîne.
- Utilisez la commande `string concat` pour combiner la première lettre de la chaîne en majuscules avec le reste de la chaîne en minuscules. 

Voici un exemple de code et de sortie avec la chaîne "bonjour" :

```Fish Shell
set str "bonjour" 
set first (string toupper $str) 
set remaining (string tolower (string substring --length 1 $str)) 
echo (string concat $first $remaining)
```

Sortie : "Bonjour"

## Plongée en profondeur 
Pour ceux qui veulent en savoir plus sur la capitalisation des chaînes de caractères en utilisant Fish Shell, voici quelques informations supplémentaires :

- La commande `string toupper` fonctionne en convertissant chaque caractère de la chaîne en sa forme majuscule correspondante. Cela signifie que les caractères spéciaux tels que les accents ne seront pas convertis.
- La commande `string substring` prend différents arguments pour délimiter la partie de la chaîne que vous souhaitez récupérer. En utilisant `--length`, vous pouvez spécifier le nombre de caractères à inclure à partir du début de la chaîne.
- Enfin, la commande `string concat` combine simplement les arguments fournis dans l'ordre dans lequel ils sont énumérés.

## Voir aussi 
- La documentation officielle de Fish Shell sur la gestion des chaînes de caractères : https://fishshell.com/docs/current/cmds/string.html 
- Un tutoriel sur l'utilisation de Fish Shell pour manipuler des chaînes de caractères : https://www.linux.com/news/string-manipulation-fish-shell/
- Un guide complet sur les fonctionnalités de Fish Shell pour les débutants : https://www.howtogeek.com/682553/how-to-use-the-fish-shell-on-linux/