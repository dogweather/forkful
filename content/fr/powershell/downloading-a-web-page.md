---
title:                "Télécharger une page web"
html_title:           "Bash: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Qu'est-ce & Pourquoi?

En termes simples, télécharger une page web signifie obtenir le HTML (et éventuellement d'autres ressources) d'une adresse web particulière. Les programmeurs font cela pour diverses raisons, comme le scrapping de données ou l'automatisation de tests sur des sites web.

## Comment faire:

Télécharger une page web avec PowerShell est assez simple. Voici un exemple de comment le faire :

```PowerShell
# Définition de l'URL
$url = "http://example.com"

# Utilise Invoke-WebRequest pour télécharger la page
$page = Invoke-WebRequest -Uri $url

# Affiche le contenu de la page
$page.Content
```
En exécutant ce script, vous devriez voir le contenu HTML de la page `example.com` s'afficher dans votre console.

## Approfondissement:

Historiquement, le téléchargement de pages web a d'abord été fait à l'aide de bibliothèques externes dans la plupart des langages de programmation. PowerShell a cependant intégré cette fonctionnalité directement dans le langage avec la cmdlet `Invoke-WebRequest` depuis sa version 3.0.

D'autres méthodes peuvent aussi être utilisées pour télécharger une page web, comme la cmdlet `Invoke-RestMethod` qui est plus adaptée pour les APIs REST, ou encore `System.Net.WebClient`, qui est un objet .NET plus bas niveau que vous pouvez utiliser directement depuis PowerShell.

Lorsque vous téléchargez une page web avec `Invoke-WebRequest`, PowerShell envoie une requête HTTP GET à l'URL spécifiée et retourne une réponse contenant plusieurs informations comme le contenu de la page, les headers HTTP, le statut de la réponse, etc. Vous pouvez accéder à ces informations en utilisant les propriétés de l'objet retourné par `Invoke-WebRequest`.

## Voir aussi:

Pour plus d'informations sur `Invoke-WebRequest` et le téléchargement de pages web avec PowerShell, vous pouvez consulter les ressources suivantes :

- [Documentation officielle de Microsoft sur Invoke-WebRequest](https://docs.microsoft.com/fr-fr/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1)
- [Tutorial sur le scrapping de données web avec PowerShell](https://adamtheautomator.com/powershell-web-scraping-tutorial/)
- [Guide sur l'utilisation de System.Net.WebClient en PowerShell](https://devblogs.microsoft.com/scripting/using-windows-powershell-to-work-with-the-net-framework-classes/)