---
title:                "PHP: Calculer une date dans le futur ou le passé"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur PHP, il est probable que vous ayez besoin de calculer une date dans le futur ou le passé à un moment donné. Peut-être que vous créez un calendrier pour un site web ou une application, ou peut-être que vous souhaitez ajouter des fonctionnalités de planification de rendez-vous pour vos utilisateurs. Quelle que soit la raison, comprendre comment calculer des dates est un élément important de la programmation en PHP.

## Comment faire

La méthode la plus simple pour calculer une date dans le futur ou le passé est d'utiliser la fonction PHP `strtotime()`. Cette fonction prend une date sous forme de chaîne de caractères et la convertit en timestamp, qui représente le nombre de secondes écoulées depuis le 1er janvier 1970 à minuit. En utilisant `strtotime()`, vous pouvez ajouter ou soustraire du nombre de secondes à une date donnée pour obtenir une date dans le futur ou le passé.

Par exemple:

```
<?php
$date = '2021-04-15';
$nouvelle_date = strtotime('+3 jours', strtotime($date));
echo date('Y-m-d', $nouvelle_date); // output: 2021-04-18
$nouvelle_date = strtotime('-1 mois', strtotime($date));
echo date('Y-m-d', $nouvelle_date); // output: 2021-03-15
```

Il est également utile de savoir que la fonction `strtotime()` peut comprendre des expressions comme "maintenant" ou "demain" pour calculer des dates en fonction de la date actuelle.

## Plongée en profondeur

La fonction `strtotime()` offre également une flexibilité pour calculer des dates basées sur des intervalles spécifiques. Par exemple, vous pouvez calculer la date dans 2 semaines, 5 mois ou 10 ans en utilisant des expressions adaptées. De plus, la fonction prend également en compte les années bissextiles lors de la conversion en timestamp.

Il est important de noter que la fonction `strtotime()` n'est pas limitée aux dates futures ou passées. Vous pouvez également utiliser des expressions pour spécifier des dates précises, telles que "premier jour du mois prochain" ou "deuxième mercredi du mois".

Enfin, il existe d'autres fonctions relatives aux dates en PHP telles que `date_add()` et `date_sub()` qui peuvent également être utilisées pour calculer des dates dans le futur ou le passé. Assurez-vous de consulter la documentation officielle de PHP pour en savoir plus sur ces fonctions et leurs possibilités.

## Voir aussi

Pour en savoir plus sur les fonctions de manipulation de dates en PHP, voici quelques liens utiles:

- Documentation officielle de la fonction `strtotime()`: https://www.php.net/manual/fr/function.strtotime.php
- Tutoriel sur les fonctions de manipulation de dates en PHP: https://www.php.net/manual/fr/datetime.formats.relative.php
- Stack Overflow: Comment calculer une date dans le futur en utilisant PHP: https://stackoverflow.com/a/7605854/14832325