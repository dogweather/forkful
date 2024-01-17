---
title:                "Obtenir la date actuelle"
html_title:           "C#: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi les programmeurs le font?

Obtenir la date actuelle dans une application est une tâche courante pour les programmeurs. Cela permet de garder une trace du moment où une action a eu lieu ou de synchroniser des données avec d'autres utilisateurs. Cela peut également être important pour des tâches telles que les sauvegardes ou le suivi des performances de l'application.

# Comment faire:

Voici un exemple de code en C# pour obtenir la date actuelle et l'afficher à l'écran:

```C#
DateTime currentDate = DateTime.Now; // Crée un objet DateTime avec la date et l'heure actuelles
Console.WriteLine("La date actuelle est: " + currentDate.ToString()); // Affiche la date actuelle à l'écran
```

Le code ci-dessus utilise la classe `DateTime` de C# pour créer un objet avec la date et l'heure actuelles. Ensuite, il utilise la méthode `ToString()` pour formater cette date en une chaîne de caractères lisible pour l'utilisateur.

# Plongez plus en profondeur:

Bien qu'il existe plusieurs façons d'obtenir la date actuelle en C#, l'utilisation de la classe `DateTime` est la plus courante. Cependant, il existe également la classe `DateTimeOffset` qui prend en compte le fuseau horaire et peut être utile pour les applications nécessitant des conversions de date internationales.

Si vous souhaitez obtenir la date et l'heure précises, il existe la méthode `UtcNow` de la classe `DateTime` qui utilise le temps universel coordonné (UTC) plutôt que le fuseau horaire local.

Enfin, pour les applications qui doivent gérer des horaires spécifiques (par exemple, les fuseaux horaires ou les heures d'été), il peut être utile d'utiliser la classe `TimeZoneInfo` pour obtenir des informations détaillées sur les fuseaux horaires.

# Voir aussi:

- [Documentation officielle de C# sur DateTime](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime)
- [Différences entre DateTime et DateTimeOffset](https://devblogs.microsoft.com/dotnet/understanding-datetimeoffset/)
- [Gestion des fuseaux horaires en C#](https://www.c-sharpcorner.com/uploadfile/dhananjaycoder/working-with-time-zone-in-C-Sharp30/)