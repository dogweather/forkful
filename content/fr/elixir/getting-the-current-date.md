---
title:    "Elixir: Obtenir la date actuelle"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Pourquoi

Il existe de nombreuses raisons pour lesquelles vous pourriez vouloir obtenir la date actuelle dans vos programmes Elixir. Cela peut vous aider à afficher des informations à jour, à créer des tâches programmées ou à effectuer des calculs basés sur la date actuelle.

# Comment faire

Il existe plusieurs façons d'obtenir la date actuelle en utilisant Elixir. Voici un exemple de code utilisant la fonction `DateTime.now()` pour obtenir la date et l'heure actuelles :

```
Elixir

current_date = DateTime.now()
IO.puts "La date actuelle est : #{current_date}"
```

Cela affichera la date et l'heure actuelles dans le format suivant :

```
La date actuelle est : 2021-06-29 10:23:00.237834
```

Vous pouvez également utiliser la fonction `NaiveDateTime.utc_now()` pour obtenir la date et l'heure actuelles en temps universel coordonné (UTC).

# Plongée plus profonde

Il est important de noter que la fonction `DateTime.now()` est basée sur l'horloge système de votre ordinateur et peut donc être affectée par des changements de fuseau horaire ou des ajustements de l'horloge. Pour obtenir une date plus fiable et constante, vous pouvez utiliser la fonction `NaiveDateTime.utc_now()` et convertir le résultat en fonction de votre fuseau horaire.

Il existe également des bibliothèques tierces telles que "Timex" ou "Calendar" qui offrent des fonctionnalités supplémentaires pour la manipulation des dates et heures en Elixir.

# Voir aussi

- Documentation officielle d'Elixir sur les dates et heures : https://hexdocs.pm/elixir/DateTime.html
- Documentation de la bibliothèque Timex : https://hexdocs.pm/timex/Timex.html
- Documentation de la bibliothèque Calendar : https://hexdocs.pm/calendar/Calendar.html