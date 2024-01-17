---
title:                "Envoyer une requête http"
html_title:           "PowerShell: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
En programmation, envoyer une requête HTTP signifie envoyer une demande à un serveur pour obtenir des informations ou effectuer une action spécifique. Les programmeurs le font pour interagir avec des sites Web, des API ou des services en ligne.

## Comment faire:
Voici un exemple de code PowerShell pour envoyer une requête HTTP vers le site web "example.com" et afficher les données de réponse:
```PowerShell
$request = Invoke-WebRequest -Uri "http://example.com"
$request.Content
```
Résultat:
```
<!DOCTYPE html>
<html>
<head>
<title>Example Domain</title>
</head>
<body>
<h1>Example Domain</h1>
<p>This domain is for use in illustrative examples in documents. You may use this
domain in literature without prior coordination or asking for permission.</p>
<p><a href="https://www.iana.org/domains/example">More information...</a></p>
</body>
</html>
```

Pour envoyer une requête avec des paramètres spécifiques, vous pouvez utiliser l'argument "-Body" et spécifier des données au format JSON:
```PowerShell
$body = @{
    username = "johnsmith"
    password = "mypassword"
} | ConvertTo-Json

Invoke-WebRequest -Uri "http://example.com/login" -Method "POST" -Body $body -ContentType "application/json"
```

## Plongée en profondeur:
L'envoi de requêtes HTTP existe depuis les débuts d'Internet et est un composant essentiel de l'architecture du Web. Il permet aux développeurs d'interagir avec des services et des données en ligne de manière simple et efficace.

En plus de PowerShell, il existe d'autres outils et langages de programmation qui permettent l'envoi de requêtes HTTP, tels que Python, JavaScript ou cURL. Ces outils peuvent être utiles pour des tâches spécifiques ou pour les développeurs qui préfèrent un langage différent.

Dans le code PowerShell, l'argument "-Method" peut être utilisé pour spécifier le type de méthode HTTP à utiliser, tel que GET, POST, PUT ou DELETE. L'argument "-Headers" permet de spécifier des en-têtes personnalisés pour la requête. Il est également possible de spécifier une authentification avec l'argument "-Credential".

## Voir aussi:
- [Documentation PowerShell sur Invoke-WebRequest](https://docs.microsoft.com/fr-fr/powershell/module/Microsoft.PowerShell.Utility/Invoke-WebRequest?view=powershell-7)
- [Analyser les données JSON dans PowerShell](https://docs.microsoft.com/fr-fr/powershell/module/Microsoft.PowerShell.Utility/ConvertFrom-Json?view=powershell-7)