---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "C#: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# À quoi ça sert et pourquoi le faire?
L'envoi d'une requête HTTP avec une authentification de base est une manière de sécuriser les échanges de données entre un client et un serveur. Les programmeurs utilisent cette méthode pour s'assurer que seuls les utilisateurs autorisés peuvent accéder à certaines ressources en ligne.

# Comment procéder:
Voici un exemple de code en C# montrant comment envoyer une requête HTTP avec une authentification de base :

```
WebClient client = new WebClient();
string credentials = Convert.ToBase64String(Encoding.ASCII.GetBytes("username:password"));
client.Headers[HttpRequestHeader.Authorization] = "Basic " + credentials;
string response = client.DownloadString("url");
Console.WriteLine(response);
```

Le code crée un WebClient, encode un nom d'utilisateur et un mot de passe en base64 et les ajoute aux en-têtes de la requête pour une authentification de base. Enfin, la réponse du serveur est téléchargée et affichée dans la console.

# Petite plongée:
L'envoi de requêtes HTTP avec une authentification de base a été introduit dans la version 1.1 du protocole HTTP. Certaines alternatives à cette méthode incluent l'utilisation de jetons d'authentification tels que JWT ou OAuth.

En ce qui concerne l'implémentation, il est important de noter que l'authentification de base n'est pas considérée comme étant très sécurisée car les informations d'authentification sont envoyées en clair dans les en-têtes de la requête. Il est donc recommandé d'utiliser cette méthode en conjonction avec d'autres mesures de sécurité.

# À voir également:
- [HTTP Basic Authentication](https://www.w3.org/Protocols/rfc2616/rfc2616-sec11.html)
- [JWT vs Basic Authentication](https://stackoverflow.com/questions/39909424/json-web-token-jwt-vs-basic-authentication)