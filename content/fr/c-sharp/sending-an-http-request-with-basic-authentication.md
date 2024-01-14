---
title:                "C#: Envoi d'une requête http avec authentification de base"
simple_title:         "Envoi d'une requête http avec authentification de base"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi
L'envoi de demandes HTTP avec une authentification de base est une méthode courante utilisée pour sécuriser les communications entre un client et un serveur. Cette méthode permet de s'assurer que seules les personnes autorisées ont accès aux données confidentielles.

## Comment Faire
L'envoi d'une demande HTTP avec une authentification de base en C# est assez simple. Tout d'abord, nous avons besoin d'importer l'espace de noms `System.Net.Http` pour travailler avec les demandes HTTP. Ensuite, nous créons une instance de la classe `HttpClient` et spécifions l'URL de notre serveur dans sa construction. Nous utilisons également la classe `HttpBasicAuthenticator` pour fournir les informations d'identification du client, telles que le nom d'utilisateur et le mot de passe.

````C#
using System.Net.Http;
using RestSharp.Authenticators;


private static readonly HttpClient client = new HttpClient();

public static async Task SendRequest()
{
    client.BaseAddress = new Uri("https://example.com");

    // Informations d'authentification
    string username = "nom_utilisateur";
    string password = "mot_de_passe";

    // Utiliser la classe HttpBasicAuthenticator pour fournir l'authentification
    client.DefaultRequestHeaders.Authorization = new HttpBasicAuthenticator(username, password);

    // Spécifiez votre méthode, votre en-tête et votre corps de demande si nécessaire
    var response = await client.GetAsync("/data");

    // Afficher la réponse
    Console.WriteLine(response);
}
````

La sortie de notre demande sera quelque chose comme ceci :

```
StatusCode: 200, ReasonPhrase: 'OK', Version: 1.1, Content: System.Net.Http.StreamContent, Headers:
{
  Date: Tue, 06 Jul 2021 00:00:00 GMT
  Server: Apache
  Content-Length: 328
  Content-Type: application/json; charset=utf-8
}
```

## En Profondeur
L'authentification de base est l'un des nombreux mécanismes d'authentification pris en charge par HTTP. Il est basé sur un protocole simple qui envoie les informations d'identification du client sous forme d'en-têtes de demande, sous forme de nom d'utilisateur et de mot de passe encodés en Base64. Bien que cette méthode soit assez facile à mettre en œuvre, elle n'est pas recommandée pour les communications sensibles, car les informations d'identification sont facilement interceptées et décodées.

Pour une sécurité renforcée, il est recommandé d'utiliser d'autres méthodes d'authentification, telles que l'authentification par jeton ou OAuth.

## Voir Aussi
- [Documentation officielle de Microsoft sur l'envoi de demandes HTTP](https://docs.microsoft.com/fr-fr/dotnet/api/system.net.http.httpclient?view=net-5.0)
- [Tutoriel vidéo sur l'envoi de demandes HTTP avec authentification en C#](https://www.youtube.com/watch?v=EBc8_hgvbX0)