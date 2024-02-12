---
title:                "Obtenir la date actuelle"
aliases:
- fr/vba/getting-the-current-date.md
date:                  2024-02-01T21:54:37.678241-07:00
model:                 gpt-4-0125-preview
simple_title:         "Obtenir la date actuelle"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/vba/getting-the-current-date.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Dans Visual Basic pour Applications (VBA), récupérer la date actuelle est une tâche courante qui permet aux programmeurs de travailler dynamiquement avec des dates dans leurs macros ou applications. Cette fonctionnalité est cruciale pour des opérations telles que la consignation, l'horodatage des transactions ou la réalisation de calculs basés sur des dates.

## Comment faire :

Récupérer la date actuelle en VBA est simple, en utilisant la fonction `Date`, tandis que la fonction `Now` fournit à la fois la date et l'heure actuelles. Voici comment vous pouvez travailler avec les deux :

```vb
Sub GetCurrentDate()
    ' Utilisation de la fonction Date pour obtenir la date actuelle
    Dim currentDate As Date
    currentDate = Date
    Debug.Print "Date Actuelle : "; currentDate
    
    ' Utilisation de la fonction Now pour obtenir la date et l'heure actuelles
    Dim currentDateTime As Date
    currentDateTime = Now
    Debug.Print "Date et Heure Actuelles : "; currentDateTime
End Sub
```

Lorsque vous exécutez cette macro, la méthode `Debug.Print` affiche la date actuelle et la date et l'heure actuelles dans la fenêtre Immédiate de l'éditeur VBA. Par exemple :

```
Date Actuelle : 12/04/2023
Date et Heure Actuelles : 12/04/2023 15:45:22
```

Gardez à l'esprit que le format de la date peut varier en fonction des paramètres système de l'ordinateur de l'utilisateur.

## Approfondissement

Les fonctions `Date` et `Now` encapsulent la complexité de la gestion des dates et des heures dans Visual Basic pour Applications, en fournissant une abstraction au niveau de l'application qui rend le travail avec les dates simple et intuitif. Historiquement, traiter des dates et des heures en programmation a été semé d'embûches, y compris la gestion des différents fuseaux horaires, des changements d'heure d'été et des divers formats de date.

Dans VBA, ces fonctions dépendent de la date et de l'heure du système sous-jacent, ce qui signifie qu'elles sont influencées par les paramètres locaux et système de l'utilisateur. C'est une épée à double tranchant qui garantit la cohérence avec l'environnement de l'utilisateur mais nécessite également une manipulation soignée de la localisation et des ajustements de fuseau horaire dans les applications globales.

Bien que les fonctions de date et d'heure de VBA soient parfaitement adaptées à de nombreuses applications, en particulier dans le cadre de l'automatisation de Office, elles peuvent manquer de la précision ou de la granularité requises pour des applications plus complexes comme les systèmes de trading à haute fréquence ou les simulations scientifiques. Dans de tels cas, d'autres environnements ou langages de programmation comme Python ou C# pourraient offrir des bibliothèques de manipulation de dates et d'heures plus sophistiquées.

Néanmoins, pour la grande majorité des tâches impliquant des dates et des heures dans le contexte d'Excel, Word ou d'autres applications Office, les fonctions `Date` et `Now` de VBA offrent un équilibre de simplicité, de performance et de facilité d'utilisation difficile à surpasser.
