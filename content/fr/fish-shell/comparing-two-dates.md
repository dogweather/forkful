---
title:                "Comparer deux dates"
html_title:           "Clojure: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Comparer deux dates, c'est simplement déterminer quelle date est la plus récente ou la plus ancienne. Et pourquoi les programmeurs font-ils cela? Pour résoudre une myriade de problèmes comme le calcul des échéances, la planification de tâches, etc.

## Comment faire:

Voici comment vous pouvez comparer deux dates en Fish Shell:
```Fish Shell
set date1 (date -u +%s -d "2022-04-01")
set date2 (date -u +%s -d "2022-04-02")
if test "$date1" -gt "$date2"
   echo "Date1 est plus récente"
else 
   echo "Date2 est plus récente"
end
```
Output:
```Fish Shell
"Date2 est plus récente"
```
Ici, le script convertit d'abord les dates en timestamp Unix (secondes écoulées depuis 1970), puis les compare.

## Approfondissement:

Historiquement, la comparaison de dates a toujours été une tâche essentielle dans le codage. Mais chaque langage de programmation a sa propre façon de le faire. Dans Fish Shell, les dates sont converties en timestamps Unix pour faciliter la comparaison. Cependant, il existe d'autres alternatives comme l'utilisation de commandes intégrées de certaines langues pour comparer directement les dates.

L'implémentation du code ci-dessus est simple. Elle fait usage de commandes Unix comme `date` et `test`. La commande `date -u +%s` produit un timestamp Unix et `-d` permet de spécifier la date. Ensuite, la commande `test` est utilisée pour comparer les deux timestamps.

## Voir aussi:

Voici quelques liens pertinents où vous pouvez en savoir plus sur le sujet:

[Documentation officielle de la Fish Shell](https://fishshell.com/docs/current/index.html)

[Comparaison de dates en Unix](https://unix.stackexchange.com/questions/2465/how-to-compare-to-a-date-in-a-shell-script)

[Détails sur les dates Unix](https://www.computerhope.com/unix/udate.htm)

Bonne programmation avec Fish Shell!