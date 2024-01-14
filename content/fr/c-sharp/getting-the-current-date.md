---
title:                "C#: Obtenir la date actuelle"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

La date et l'heure actuelles sont des informations importantes dans la vie de tous les jours. Les programmeurs utilisent souvent cette information pour enregistrer des données, créer des horaires ou simplement pour afficher l'heure actuelle dans leur programme. Dans cet article, nous allons découvrir comment obtenir la date et l'heure actuelles en utilisant le langage de programmation C#.

## Comment faire

Pour obtenir la date et l'heure actuelles en C#, nous utilisons la classe `DateTime`. Cette classe contient de nombreuses méthodes et propriétés qui nous permettent de manipuler facilement les dates et heures.

```
C# var dateActuelle = DateTime.Now; 
Console.WriteLine(dateActuelle); //Output: 06/07/2021 14:30:00
```

Dans l'exemple ci-dessus, nous déclarons une variable `dateActuelle` de type `DateTime` et nous utilisons la propriété `Now` pour récupérer la date et l'heure actuelles. Ensuite, nous utilisons la méthode `WriteLine` pour afficher le résultat à l'écran. La sortie sera dans le format suivant : "MM / dd / yyyy HH: mm: ss".

Nous pouvons également personnaliser le format de sortie en utilisant la méthode `ToString` avec un format spécifique en utilisant les symboles de formatage de date et heure.

```
C# var dateActuelle = DateTime.Now; 
Console.WriteLine(dateActuelle.ToString("dd-MM-yyyy")); //Output: 07-06-2021
```

Il est également possible d'obtenir des informations spécifiques telles que le jour, le mois, l'année ou même le fuseau horaire en utilisant les propriétés de la classe `DateTime`.

```
C# var dateActuelle = DateTime.Now; 
Console.WriteLine(dateActuelle.Day); //Output: 07
Console.WriteLine(dateActuelle.Month); //Output: 06
Console.WriteLine(dateActuelle.Year); //Output: 2021
Console.WriteLine(dateActuelle.TimeOfDay); //Output: 14:30:00
```

## Plongée en profondeur

Maintenant que nous savons comment obtenir la date et l'heure actuelles en C#, il est important de comprendre comment cela fonctionne. Lorsque nous appelons la méthode `Now` ou toute autre méthode de la classe `DateTime`, le système d'exploitation fournira l'heure actuelle en fonction des paramètres régionaux du système. Cela signifie que si nous changeons les paramètres régionaux, nous obtiendrons des résultats différents.

Il existe également d'autres méthodes dans la classe `DateTime` qui nous permettent de manipuler les dates et heures. Par exemple, la méthode `Add` nous permet d'ajouter une période spécifique à une date donnée, tandis que la méthode `Subtract` nous permet de soustraire une période.

## Voir aussi

Pour en savoir plus sur la classe `DateTime` en C# et ses différentes méthodes et propriétés, consultez la documentation officielle de Microsoft sur [DateTime](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime?view=net-5.0).

Vous pouvez également consulter nos autres articles sur le langage de programmation C# en suivant l'un des liens ci-dessous :

- [Comment créer et utiliser des constantes en C#](https://monsite.com/article/constantes-c-sharp)
- [Les bases du traitement des exceptions en C#](https://monsite.com/article/exceptions-c-sharp)
- [Manipuler des chaînes de caractères en C#](https://monsite.com/article/chaines-caracteres-c-sharp)