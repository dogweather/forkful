---
title:                "Téléchargement d'une page web"
html_title:           "PowerShell: Téléchargement d'une page web"
simple_title:         "Téléchargement d'une page web"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

Le téléchargement de pages Web en utilisant PowerShell

## Qu'est-ce que c'est et pourquoi les programmeurs le font-ils?

Le téléchargement d'une page Web consiste en la récupération du contenu HTML d'une URL spécifiée. Les programmeurs utilisent cette technique pour extraire des données à partir de sites Web, automatiser des tâches de gestion de contenu ou développer des scripts de surveillance de site. PowerShell offre des fonctionnalités intégrées pour effectuer des téléchargements de pages Web de manière rapide et efficace.

## Comment le faire:

Exemple de code pour télécharger une page Web à l'aide de PowerShell:

```
$url = "https://www.example.com/"
$output = Invoke-WebRequest -Uri $url
$output.Content
```

Cet exemple va télécharger le contenu de la page Web en utilisant la commande `Invoke-WebRequest` et le stocker dans la variable `$output`. Ensuite, le contenu de la page sera affiché à l'écran en utilisant la propriété `Content` de l'objet `$output`.

## Plongée en profondeur:

Il est important de noter que PowerShell utilise Internet Explorer comme navigateur par défaut pour effectuer des téléchargements de pages Web. Cela signifie que les sites Web peuvent détecter que la demande provient d'un navigateur et bloquer l'accès si elles le souhaitent. Pour contourner cela, vous pouvez utiliser un en-tête utilisateur personnalisé lors de l'appel de `Invoke-WebRequest`.

Il existe également d'autres méthodes pour télécharger des pages Web en utilisant des modules tiers tels que `PowershellGet`. Cependant, pour des besoins simples, l'utilisation de `Invoke-WebRequest` suffit la plupart du temps.

Pour en savoir plus sur la commande `Invoke-WebRequest` et ses options, vous pouvez consulter la documentation officielle de Microsoft.

## Voir aussi:

- [Documentation officielle de Microsoft pour `Invoke-WebRequest`](https://docs.microsoft.com/fr-fr/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1)
- [Article de blog sur l'utilisation de `Invoke-WebRequest` en tant que bot de recherche de mots-clés](https://ironscripter.us/using-powershell-as-a-web-bot/)