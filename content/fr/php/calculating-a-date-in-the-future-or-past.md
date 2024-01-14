---
title:    "PHP: Calcul d'une date dans le futur ou le passé."
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Pourquoi

Il y a plusieurs raisons pour lesquelles vous pourriez avoir besoin de calculer une date dans le futur ou le passé en PHP. Par exemple, vous pourriez être en train de créer une application de réservation ou de planification, ou tout simplement besoin de suivre des dates pour une tâche spécifique.

# Comment Faire

Pour calculer une date dans le futur ou le passé en PHP, vous pouvez utiliser la fonction `strtotime()` en lui passant une chaîne de date et une date de référence en tant que paramètres. Par exemple:

```PHP
$future_date = strtotime('next week');
echo date('d/m/Y', $future_date); // outputs: 16/03/2020
$past_date = strtotime('last month');
echo date('d/m/Y', $past_date); // outputs: 01/02/2020
```

Vous pouvez également ajouter ou soustraire des valeurs à la date de référence en utilisant des codes spécifiques. Par exemple:

```PHP
$future_date = strtotime('+2 years');
echo date('d/m/Y', $future_date); // outputs: 14/03/2022
$past_date = strtotime('-3 days');
echo date('d/m/Y', $past_date); // outputs: 09/03/2020
```

# Plongeons Plus Profondément

Pour une flexibilité et une précision supplémentaires, vous pouvez également utiliser la classe `DateTime` de PHP pour calculer des dates. Voici un exemple:

```PHP
$date = new DateTime();
$date->modify('+2 weeks');
echo $date->format('d/m/Y'); // outputs: 30/03/2020
```

De plus, vous pouvez utiliser différentes options de formatage telles que `createFromFormat()` ou `diff()` pour des calculs de dates plus complexes. Il est important de noter que le format par défaut de `DateTime` est `Y-m-d H:i:s`, qui peut être modifié en utilisant la méthode `setDate()`.

# Voir Aussi

- [Documentation officielle de PHP sur la fonction strtotime()](https://www.php.net/manual/fr/function.strtotime.php)
- [Tutoriel sur la classe DateTime en PHP](https://www.tutorialspoint.com/php/php_date_time.htm)
- [Guide complet sur la manipulation des dates en PHP](https://www.sitepoint.com/managing-dates-times-php/)