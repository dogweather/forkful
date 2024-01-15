---
title:                "Comparaison de deux dates"
html_title:           "C#: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi
Si vous travaillez avec des données temporelles dans vos programmes en C#, il est courant de devoir comparer deux dates. Cela peut être utile pour effectuer des calculs, trier des données ou simplement vérifier si une date se situe avant ou après une autre.

## Comment faire
Pour comparer deux dates en C#, vous pouvez utiliser la méthode `Compare` de la classe `DateTime`. Cette méthode retourne un nombre qui indique si la première date est antérieure, égale ou postérieure à la seconde date. Voici un exemple de code :

```C#
// Déclaration de deux dates
DateTime date1 = new DateTime(2021, 07, 15);
DateTime date2 = new DateTime(2021, 07, 20);

// Comparaison des deux dates
int result = DateTime.Compare(date1, date2);

// Affichage du résultat
if (result == 0)
{
    Console.WriteLine("Les deux dates sont égales.");
}
else if (result < 0)
{
    Console.WriteLine("La première date est antérieure à la seconde date.");
}
else
{
    Console.WriteLine("La première date est postérieure à la seconde date.");
}
// Output : "La première date est antérieure à la seconde date."
```

Outre la méthode `Compare`, il existe également d'autres méthodes pour comparer des dates en C#, telles que `Equals`, `CompareTo` ou encore les opérateurs `==` et `>`. N'hésitez pas à les explorer et à choisir celle qui convient le mieux à votre situation.

## Plongée en profondeur
Lors de la comparaison de dates, il est important de prendre en compte des éléments tels que le fuseau horaire et le format de date. Par exemple, si vous utilisez la méthode `Equals`, elle se basera sur l'exactitude à la minute près et ne tiendra pas compte du fuseau horaire. Il est donc important de bien comprendre les différentes méthodes de comparaison afin d'utiliser celle qui correspond à vos besoins.

De plus, il peut également être utile de convertir les dates en une représentation standard, telle que les secondes depuis le 1er janvier 1970 (appelée "Epoch time"). Cela facilitera grandement les comparaisons et les opérations de calcul.

## Voir aussi
- [Documentation officielle sur la classe DateTime](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime?view=net-5.0)
- [Comparaison de dates en C#](https://www.c-sharpcorner.com/UploadFile/84c85b/comparing-datetimes-using-ldquo-comparerdquo-class-in-C-Sharp/)
- [Comment convertir une date en Epoch time en C#](https://www.c-sharpcorner.com/UploadFile/84c85b/converting-date-to-epoch-time-using-C-Sharp/)