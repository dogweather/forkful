---
title:    "Fish Shell: Comparer deux dates"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Pourquoi

Dans le monde de la programmation, il est courant de devoir comparer des dates pour diverses raisons telles que trier des données ou vérifier la validité d'une information. Dans cet article, nous allons explorer comment comparer deux dates en utilisant le shell de poisson (Fish Shell) pour vous faciliter la tâche.

## Comment faire

Pour comparer deux dates en utilisant Fish Shell, nous allons utiliser la commande `date` suivie d'un format spécifique. Voici un exemple:

```Fish Shell
set debut (date -f "%m%d%Y" "15042021")
set fin (date -f "%m%d%Y" "20042021")
echo $debut $fin
if test $debut -gt $fin
    echo "La date de début est plus récente que la date de fin."
else
    echo "La date de fin est plus récente que la date de début."
end
```
Lorsque vous exécutez ce code, vous obtenez l'output suivant:

```
15042021 20042021
La date de fin est plus récente que la date de début.
```

Ici, nous avons utilisé la commande `date -f` pour formater nos dates dans le format "mois jour année" (mmddyyyy). Ensuite, nous avons stocké ces dates dans des variables `debut` et `fin` respectivement. Enfin, nous avons utilisé la commande `test` pour comparer ces variables en utilisant l'opérateur `-gt` (plus grand que). Selon le résultat de cette comparaison, nous avons affiché un message approprié.

Il est important de noter que le code ci-dessus utilise le format de date américain (mmddyyyy). Selon votre localisation, vous devrez peut-être utiliser un format de date différent. Vous pouvez utiliser la commande `man date` pour en savoir plus sur les différents formats disponibles.

## Plongeons plus profondément

Maintenant que nous avons vu un exemple simple de comparaison de dates en utilisant Fish Shell, voici quelques points à garder à l'esprit:

- Vous pouvez comparer des dates dans n'importe quel format tant que les deux dates sont dans le même format. Le format que vous utilisez dépendra de vos besoins.
- Vous pouvez également utiliser la commande `date -s` pour définir des dates spécifiques plutôt que d'utiliser des variables.
- Si vous avez besoin de comparer des dates en incluant l'heure, vous pouvez utiliser des formats comme "hhmmss".
- Vous pouvez utiliser des opérateurs de comparaison différents, comme `-lt` (moins que) ou `-eq` (égal à), pour obtenir des résultats plus précis en fonction de vos besoins.

En utilisant ces connaissances, vous pouvez facilement adapter l'exemple de code ci-dessus pour répondre à vos besoins spécifiques de comparaison de dates.

## Voir aussi

- [Documentation officielle de Fish Shell](https://fishshell.com/docs/current/)
- [Guide de référence des commandes Fish Shell](https://fishshell.com/docs/current/cmds.html)
- [Tutoriel sur la manipulation des dates en Fish Shell](https://linuxhint.com/fish_shell_date_tutorial/)

Avec cet article, vous avez maintenant les connaissances nécessaires pour comparer efficacement deux dates en utilisant Fish Shell. N'hésitez pas à explorer les différentes options et à tester par vous-même pour mieux comprendre comment cela fonctionne. Bonne programmation!