---
title:    "PHP: Transformer une date en une chaîne de caractères"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez pourquoi vous devriez apprendre à convertir une date en une chaîne de caractères en PHP? Eh bien, cette compétence peut être très utile lors de la manipulation de dates dans vos projets de développement. En utilisant cette fonction, vous pourrez formater et présenter les dates de manière plus lisible et adaptée à votre public.

## Comment faire

Pour convertir une date en une chaîne de caractères en PHP, vous pouvez utiliser la fonction `date()`. Passons en revue quelques exemples pour mieux comprendre comment cela fonctionne.

```PHP
$date = mktime(0, 0, 0, 12, 1, 2021); // Crée un timestamp pour le 1er décembre 2021
echo date("d/m/Y", $date); // Affiche la date formatée: 01/12/2021
echo date("l", $date); // Affiche le jour de la semaine: mercredi
```

Dans cet exemple, nous avons utilisé la fonction `mktime()` pour créer un timestamp représentant le 1er décembre 2021. Ensuite, nous avons passé ce timestamp en paramètre à la fonction `date()`, en spécifiant le format de date souhaité en utilisant des lettres spéciales telles que `d` pour le jour, `m` pour le mois et `Y` pour l'année. Nous pouvons également spécifier des lettres pour afficher le jour de la semaine, le numéro du mois, etc.

```PHP
$date = strtotime("2021-12-01"); // Convertit la date en timestamp
echo date("d/m/Y", $date); // Affiche la date formatée: 01/12/2021
```

Dans cet exemple, nous avons utilisé la fonction `strtotime()` pour convertir directement la date en un timestamp. Ensuite, nous avons utilisé la fonction `date()` pour formater ce timestamp en une chaîne de caractères dans le format souhaité.

## Approfondissement

Il existe de nombreuses autres options et formats disponibles pour la fonction `date()` en plus de ceux mentionnés dans les exemples ci-dessus. Vous pouvez consulter la documentation officielle de PHP pour en savoir plus sur cette fonction et toutes ses possibilités.

De plus, il existe également une fonction `strftime()` qui offre encore plus d'options de formatage pour les dates en PHP. Cette fonction peut être utile lorsque vous devez prendre en compte les paramètres régionaux et les langues de votre public.

N'oubliez pas que la fonction `date()` convertit une date en une chaîne de caractères, ce qui signifie que si vous avez besoin de travailler avec des dates en tant qu'objets, vous devrez utiliser d'autres fonctions telles que `strtotime()` ou `DateTime`.

## Voir aussi

- [Documentation de la fonction date() en PHP](https://www.php.net/manual/en/function.date.php)
- [Documentation de la fonction strftime() en PHP](https://www.php.net/manual/en/function.strftime.php)
- [Documentation de la fonction strtotime() en PHP](https://www.php.net/manual/en/function.strtotime.php)