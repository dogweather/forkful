---
title:                "C#: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Pourquoi

Les requêtes HTTP sont un élément fondamental de la programmation et sont utilisées pour communiquer avec des serveurs et récupérer des données. Elles sont particulièrement utiles pour construire des applications web dynamiques et interactives. Apprendre à envoyer une requête HTTP en C# est donc une compétence précieuse pour tout programmeur.

## Comment Faire

Pour envoyer une requête HTTP en C#, nous allons utiliser la classe HttpClient de la bibliothèque standard System.Net.Http. Tout d'abord, nous devons créer une instance de cette classe en utilisant le mot-clé `new`. Ensuite, nous pouvons utiliser la méthode `GetAsync()` pour spécifier l'adresse URL de la requête que nous souhaitons envoyer. Enfin, nous pouvons appeler la méthode `Result` sur l'objet retourné pour obtenir la réponse de la requête. Voici un exemple de code pour envoyer une requête GET en utilisant HttpClient:

```C#
var client = new HttpClient();
var response = client.GetAsync("https://www.example.com").Result;
```

Le résultat de cette requête peut être traité et utilisé pour afficher les données de la réponse, telles que le contenu HTML d'une page web.

## Plongée Profonde

En plus de la méthode `GetAsync()`, la classe HttpClient dispose d'autres méthodes utiles pour envoyer des requêtes telles que `PostAsync()` pour envoyer des données à un serveur, ou `SendAsync()` pour envoyer une requête avec une méthode HTTP personnalisée. De plus, nous pouvons ajouter des en-têtes ou des paramètres à la requête en utilisant les classes HttpRequestHeaders et HttpContent respectivement.

Il est également important de noter que la classe HttpClient gère de manière transparente l'utilisation des connexions persistantes en réutilisant les connexions existantes pour des requêtes vers le même serveur.

## Voir Aussi

Pour en savoir plus sur les requêtes HTTP en C#, vous pouvez consulter les ressources suivantes:

- Documentation Microsoft pour la classe HttpClient en C#: https://docs.microsoft.com/fr-fr/dotnet/api/system.net.http.httpclient
- Tutoriel sur les requêtes HTTP en C#: https://www.tutorialspoint.com/csharp/csharp_web_api.htm
- Exemples pratiques d'utilisation de HttpClient en C#: https://dotnetcoretutorials.com/2017/05/20/making-http-requests-asp-net-core-using-httpclient/