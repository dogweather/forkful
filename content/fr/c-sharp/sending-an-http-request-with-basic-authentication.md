---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Arduino: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?

Envoyer une requête HTTP avec une authentification de base, c'est fournir des identifiants sous forme de nom d'utilisateur et mot de passe pour accéder à certaines ressources. Les développeurs le font pour protéger les données sensibles d'une application.

## Comment faire :

Voici comment le faire en C#:

```C#
using System;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;

class Program
{
    private static readonly HttpClient client = new HttpClient();

    static async Task Main(string[] args)
    {
        var byteArray = Encoding.ASCII.GetBytes("username:password");
        client.DefaultRequestHeaders.Authorization 
		     = new System.Net.Http.Headers.AuthenticationHeaderValue("Basic", Convert.ToBase64String(byteArray));

        var response = await client.GetAsync("https://exemple.com");
        var responseString = await response.Content.ReadAsStringAsync();

        Console.WriteLine(responseString);
    }
}
```
Si tout se passe bien, vous devriez voir la réponse de "https://exemple.com" affichée dans votre console.

## Exploration en Profondeur

L'authentification de base HTTP est un mécanisme d'authentification couramment utilisé car il est simple à mettre en œuvre. Cependant, il est moins sécurisé que des alternatives plus modernes comme l'authentification à jetons.

Le code ci-dessus est assez direct. Il crée un en-tête d'autorisation avec des informations codées en base64 pour notre demande HTTP. Le code utilise HttpClient qui est la façon recommandée d'envoyer des requêtes HTTP en C# depuis .NET 4.5.

## Voir Aussi

Visitez les liens ci-dessous pour plus d'informations :
- [Documentation officielle de Microsoft sur HttpClient](https://docs.microsoft.com/fr-fr/dotnet/api/system.net.http.httpclient)
- [Pourquoi utiliser l'authentification Basic over Digest](https://security.stackexchange.com/questions/9907/basic-vs-digest-authentication-over-http)