---
title:    "Fish Shell: Comparer deux dates"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans le monde de la programmation, il est souvent nécessaire de comparer deux dates pour effectuer certaines actions. Peut-être que vous devez vérifier si une date est antérieure ou postérieure à une autre, ou peut-être que vous devez calculer la différence entre les deux dates. Quelle que soit la raison, la comparaison de deux dates est une tâche courante dans le développement de logiciels.

## Comment faire

La comparaison de deux dates peut sembler compliquée, mais heureusement, il existe un moyen simple de le faire en utilisant Fish Shell. Voyons ensemble comment cela fonctionne en utilisant un exemple de code:

```Fish Shell
function compare_dates -a first_date second_date
    if [ $first_date = $second_date ]
        echo "Les deux dates sont identiques"
    else if [ $first_date -lt $second_date ]
        echo "La première date est antérieure à la seconde"
    else
        echo "La première date est postérieure à la seconde"
    end
end

compare_dates 2021-02-15 2021-02-17
```

Ici, nous avons créé une fonction appelée "compare_dates" qui prend deux paramètres de date. En utilisant l'opérateur "-lt" (inférieur à), nous comparons les deux dates et affichons un message en conséquence. En exécutant cette fonction avec les dates "2021-02-15" et "2021-02-17", nous obtenons la sortie suivante:

```
La première date est antérieure à la seconde
```

Maintenant, vous pouvez utiliser cette fonction pour comparer n'importe quelles dates dans vos scripts Fish Shell.

## Plongée en profondeur

Maintenant que vous savez comment comparer deux dates en utilisant Fish Shell, il est important de comprendre comment cela fonctionne réellement. Lorsque vous utilisez l'opérateur "-lt" pour comparer des chaînes de caractères, il utilise l'ordre alphabétique pour déterminer la comparaison. Cela signifie que "2021-02-15" sera considéré comme inférieur à "2021-02-17", car le "1" est avant le "7" dans l'ordre alphabétique.

Cependant, si vous voulez comparer les dates en utilisant leur valeur numérique, vous devrez d'abord les convertir en nombres en utilisant une commande telle que "date -f %s $date". Cela convertira la date en format de timestamp, qui peut ensuite être comparé numériquement.

## Voir aussi

- [Documentation Fish Shell sur les tableaux et les chaînes de caractères](https://fishshell.com/docs/current/index.html#usage-arrays-strings)
- [Tutoriel permettant de comprendre les opérateurs de comparaison en Fish Shell](https://dev.to/chinchang/fish-shell-operators-cheatsheet-3jb9)