---
title:                "Envoyer une requête http"
html_title:           "Bash: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et Pourquoi ?

Envoyer une demande HTTP, c'est communiquer avec un serveur en utilisant le protocole HTTP (HyperText Transfer Protocol). Les programmeurs le font pour interagir avec des sites web, des API et d'autres services en ligne.

## Comment faire :

Voici comment vous pouvez envoyer une demande HTTP en utilisant PowerShell.

```PowerShell
# Importe le module
Import-Module PowerShellISE

# Crée une requête HTTP GET
$requête = Invoke-WebRequest -Uri "https://votresite.com" 

# Affiche le contenu de la réponse
$requête.Content
```

Le code ci-dessus importe le module PowerShellISE, envoie une requête HTTP GET à "https://votresite.com", puis affiche le contenu de la réponse.

Voici comment vous pouvez envoyer une requête HTTP POST.

```PowerShell
# Importe le module
Import-Module PowerShellISE

# Crée une requête HTTP POST
$requête = Invoke-WebRequest -Uri "https://votresite.com" -Method POST -Body "param1=valeur1&param2=valeur2"

# Affiche le contenu de la réponse
$requête.Content 
```

La requête HTTP POST inclut des données (dans le corps de la requête) que le serveur peut utiliser pour effectuer une action spécifique.

## Plongée en profondeur 

L'envoi de requêtes HTTP a commencé avec la création du protocole HTTP en 1989. C'est un élément essentiel du web et son évolution a été dictée par les besoins des développeurs et les avancées technologiques.

Dans PowerShell, `Invoke-WebRequest` est la méthode principale pour envoyer des requêtes HTTP. Toutefois, il existe des alternatives comme `System.Net.WebClient` ou `System.Net.Http.HttpClient` qui peuvent convenir pour des scénarios plus spécifiques.

Lors de l'envoi d'une requête HTTP, PowerShell fait plus que simplement envoyer la requête et recevoir la réponse. Il construit également des objets PowerShell autour des informations renvoyées, vous offrant la possibilité de manipuler et d'interagir de manière programmatique avec les réponses.

## Voir aussi 

Pour plus d'informations, jetez un oeil à ces sources :

- ["Invoke-WebRequest" sur Microsoft Docs](https://docs.microsoft.com/fr-fr/powershell/module/microsoft.powershell.utility/invoke-webrequest)