---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Arduino: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

---
articleTitle: Comment envoyer une requête HTTP avec une authentification de base en PowerShell
---

## Qu'est-ce que c'est et pourquoi?

En programmation, l'envoi d'une requête HTTP avec authentification de base est le processus permettant à une application client de fournir son identité à un serveur via un en-tête HTTP. C'est couramment utilisé pour accéder à des ressources nécessitant une connexion sécurisée.

## Comment faire : 
```PowerShell
# Import le module
Import-Module BitsTransfer

# Définir les informations d'identification
$user = "votre_identifiant"
$pwd = ConvertTo-SecureString "votre_mot_de_passe" -AsPlainText -Force
$credential = New-Object System.Management.Automation.PSCredential ($user, $pwd)

# Envoyer la requête
$uri = "https://adresse.de/votre/serveur"
$response = Invoke-WebRequest -Uri $uri -Method Get -Credential $credential

# Afficher le corps de la réponse
$response.Content
```

En lançant ce script, vous enverrez une requête GET à l'adresse spécifiée, avec l'authentification de base, et vous afficherez le corps de la réponse HTTP obtenue.

## Plongée en profondeur

Historiquement, l'authentification de base HTTP est l'une des premières méthodes d'authentification, définie par le protocole HTTP lui-même. Cependant, elle présente des inconvénients en termes de sécurité, car elle transmet les identifiants en clair (bien que codés en base64).

Les alternatives communes à l'authentification de base comprennent l'authentification par jeton (comme JWT) et l'authentification par défi-réponse (comme Digest). Ces méthodes offrent davantage de sécurité mais sont plus difficiles à mettre en œuvre.

En PowerShell, l'envoi d'une requête HTTP avec authentification de base est géré par la fonction Invoke-WebRequest. Cette fonction utilise les paramètres `-Uri`, `-Method` et `-Credential` pour construire la requête. 

## Voir aussi 
[Documentation officielle Microsoft sur Invoke-WebRequest](https://docs.microsoft.com/fr-fr/powershell/module/microsoft.powershell.utility/invoke-webrequest)
[Tutoriel sur l'authentification de base HTTP (en anglais)](https://www.tutorialspoint.com/http/http_authentication.htm)
[Tutoriel sur l'authentification JWT (en anglais)](https://jwt.io/introduction/)