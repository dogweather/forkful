---
title:    "C#: Obtenir la date actuelle"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

La récupération de la date actuelle est une tâche courante lors de la programmation en C#. Ce processus peut sembler simple, mais comprendre comment obtenir la date actuelle peut être utile pour de nombreuses applications, telles que le suivi du temps de traitement ou la gestion d'abonnements basés sur la date.

## Comment faire

Pour obtenir la date actuelle en C#, il existe plusieurs méthodes disponibles selon vos besoins.

- Pour obtenir la date actuelle sous forme de chaîne de caractères dans un format spécifique, utilisez la méthode `DateTime.Now.ToString("dd/MM/yyyy")`.
   ```C#
       string currentDate = DateTime.Now.ToString("dd/MM/yyyy");
       Console.WriteLine(currentDate);
   ```
   Résultat:
   ```
   23/02/2021
   ```

- Si vous avez besoin de la date actuelle dans un format par défaut, vous pouvez utiliser simplement la propriété `DateTime.Now`.
   ```C#
       DateTime currentDate = DateTime.Now;
       Console.WriteLine(currentDate);
   ```
   Résultat:
   ```
   23/02/2021 11:54:12
   ```

- Pour obtenir la date actuelle sous forme de nombre entier, utilisez la propriété `DateTime.Now.Day`, `DateTime.Now.Month` et `DateTime.Now.Year`.
   ```C#
       int day = DateTime.Now.Day;
       int month = DateTime.Now.Month;
       int year = DateTime.Now.Year;

       Console.WriteLine($"{day}/{month}/{year}");
   ```
   Résultat:
   ```
   23/02/2021
   ```

## Deep Dive

Maintenant que vous savez comment obtenir la date actuelle en C#, voici quelques informations supplémentaires à connaître:

- La méthode `DateTime.Now` retourne la date et l'heure actuelles de votre système selon le fuseau horaire local. Si vous avez besoin de la date et de l'heure dans un fuseau horaire spécifique, vous pouvez utiliser la méthode `DateTime.UtcNow` et spécifier le fuseau horaire désiré.
- Si vous avez besoin de manipuler la date et l'heure, la classe `DateTime` offre de nombreuses méthodes utiles telles que `AddDays()`, `AddHours()`, `AddMinutes()`, etc.
- Il est également possible d'utiliser la méthode `DateTime.Now.ToShortDateString()` pour obtenir uniquement la date sous forme de chaîne de caractères dans un format court.

## Voir aussi

Maintenant que vous savez comment obtenir la date actuelle en C#, voici quelques ressources supplémentaires pour approfondir vos connaissances:

- [Documentation Microsoft sur DateTime (en français)](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime?view=net-5.0)
- [Tutoriel complet pour la manipulation des dates et heures en C# (en français)](https://docs.microsoft.com/fr-fr/dotnet/csharp/programming-guide/dates-and-times/)