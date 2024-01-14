---
title:    "Bash: Utiliser les expressions régulières"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Pourquoi

Les expressions régulières sont un outil puissant pour gérer et manipuler des chaînes de caractères. Elles sont souvent utilisées en programmation pour trouver des motifs ou des correspondances dans du texte, ce qui les rend utiles pour des tâches telles que la validation de formulaires, l'analyse de données et bien plus encore.

## Comment Faire

Pour utiliser des expressions régulières en Bash, vous devez d'abord les déclarer à l'aide de l'opérateur `=~`. Voici un exemple de code qui vérifie si une chaîne de caractères correspond à un motif spécifique :

```Bash
string="Bonjour le monde"
if [[ $string =~ "Bonjour" ]]; then
  echo "La chaîne correspond au motif 'Bonjour'"
fi
```

Dans cet exemple, nous utilisons l'opérateur `=~` pour vérifier si la chaîne de caractères `$string` contient le mot "Bonjour". Si c'est le cas, nous affichons un message à l'écran.

Vous pouvez également utiliser des expressions régulières pour extraire des informations spécifiques d'une chaîne de caractères. Par exemple, si nous avons une chaîne de caractères contenant un numéro de téléphone, nous pouvons utiliser une expression régulière pour en extraire le code régional :

```Bash
phone_number="555-555-5555"
if [[ $phone_number =~ ([0-9]{3})-([0-9]{3})-([0-9]{4}) ]]; then
  echo "Code régional: ${BASH_REMATCH[1]}"
fi
```

Dans cet exemple, nous utilisons une expression régulière pour diviser la chaîne de caractères en trois parties, correspondant au code régional, au préfixe et au numéro de téléphone. Les résultats sont stockés dans un tableau `$BASH_REMATCH` et nous utilisons l'index `[1]` pour afficher uniquement le code régional.

## Plongée Profonde

Les expressions régulières en Bash sont basées sur les mêmes principes que les expressions régulières utilisées dans d'autres langages de programmation, telles que Perl ou Python. Cela signifie que les mêmes règles et symboles s'appliquent, comme le point `.` pour représenter n'importe quel caractère et l'astérisque `*` pour indiquer une répétition.

Cependant, il est important de noter que les expressions régulières en Bash ont des particularités propres à ce langage. Par exemple, elles ne prennent pas en compte les accents et les majuscules/minuscules, à moins que vous n'utilisiez l'option `-i` pour une recherche insensible à la casse.

De plus, il existe plusieurs options que vous pouvez utiliser avec l'opérateur `=~` pour modifier le comportement des expressions régulières, telles que `-n` pour que la recherche s'arrête après la première correspondance ou `-v` pour n'afficher que les non-correspondances.

Pour en savoir plus sur les expressions régulières en Bash, consultez la page de manuel `regex(7)` ou l'article de la communauté Debian [Regular Expressions](https://wiki.debian.org/Regex).

## Voir Aussi

- [Introduction aux Expressions Régulières](https://www.regular-expressions.info/fr/)
- [Apprendre les Expressions Régulières en Bash](https://www.gnu.org/software/sed/manual/html_node/Using-Regular-Expressions.html)
- [Tutoriel Bash sur les Expressions Régulières](https://bash.cyberciti.biz/guide/Main_Page)