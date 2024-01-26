---
title:                "Obtenir la date actuelle"
date:                  2024-01-20T15:13:25.102899-07:00
html_title:           "C: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Récupérer la date actuelle en C#, c'est comme demander quelle page du calendrier nous sommes. C'est utile pour des logs, des timestamps, ou des fonctionnalités liées au temps comme des rappels ou des archives.

## How to:

Voici comment obtenir la date et l'heure actuelles en C# :

```C#
using System;

class Program
{
    static void Main()
    {
        DateTime now = DateTime.Now;
        Console.WriteLine(now.ToString());
    }
}
```

Sortie possible :

```
31/03/2023 10:24:56
```

Pour seulement la date :

```C#
Console.WriteLine(now.ToShortDateString());
```

Sortie :

```
31/03/2023
```

## Deep Dive

Historiquement, la gestion des dates et heures en informatique a toujours été cruciale, pour des raisons de planification, de coordination ou pour des mesures de performance. En C#, `DateTime` est le type principal pour gérer la date et l'heure. 

Alternatives : Vous pouvez utiliser `DateTimeOffset` pour inclure des informations sur le fuseau horaire. `DateTime.UtcNow` donne l'heure universelle (UT), pratique pour une utilisation à l'échelle mondiale.

Détails d'implémentation : `DateTime.Now` est un appel système qui demande l'heure et la date courantes du système d'exploitation. Cette méthode considère le fuseau horaire local. 

Attention aux performances : appeler `DateTime.Now` peut être lent si fait fréquemment dans une boucle serrée. Dans ces cas-là, utilisez une valeur mise en cache ou `DateTime.UtcNow` si le fuseau horaire n'est pas pertinent.

## See Also

Pour creuser plus profond :

- [Documentation officielle sur DateTime](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime?view=net-6.0)
- [Meilleures pratiques pour utiliser DateTime et DateTimeOffset](https://docs.microsoft.com/fr-fr/dotnet/standard/datetime/choosing-between-datetime)
- [Information sur `DateTime.UtcNow` vs `DateTime.Now`](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime.utcnow?view=net-6.0)
