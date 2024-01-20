---
title:                "Envoyer une requête http"
html_title:           "Fish Shell: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Envoyer une Requête HTTP en C#

## Quoi & pourquoi ?

Envoyer une requête HTTP, c'est comme demander des informations à un serveur web. Les programmeurs le font pour communiquer avec des APIs, récupérer des données, ou soumettre des formulaires par exemple.

## Comment faire :

Voici un exemple de code en C# pour envoyer une requête HTTP GET à un serveur.

```csharp
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    private static readonly HttpClient client = new HttpClient();

    static async Task Main(string[] args)
    {
        var responseString = await client.GetStringAsync("http://www.example.com");

        Console.WriteLine(responseString);
    }
}
```

Et voici comment vous pouvez envoyer une requête HTTP POST avec un contenu JSON.

```csharp
using System;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;

class Program
{
    private static readonly HttpClient client = new HttpClient();

    static async Task Main(string[] args)
    {
        var content = new StringContent("{\"key\":\"value\"}", Encoding.UTF8, "application/json");
        
        var response = await client.PostAsync("http://www.example.com", content);

        var responseString = await response.Content.ReadAsStringAsync();

        Console.WriteLine(responseString);
    }
}
```

## Plongée profonde :

Historiquement, le protocole HTTP a été créé pour faciliter la communication entre les clients web (navigateurs) et les serveurs web. C# .NET fait partie des nombreux langages qui fournissent des moyens de travailler avec ce protocole.

Comme alternatives, il existe plusieurs bibliothèques tierces en C# comme RestSharp ou Flurl qui offrent des fonctionnalités supplémentaires.

En ce qui concerne les détails d'implémentation, l'objet `HttpClient` de C# est un client HTTP moderne qui fournit un moyen d'envoyer des requêtes HTTP et de recevoir des réponses HTTP.

## Voir aussi :

- [Documentation officielle de HttpClient](https://docs.microsoft.com/fr-fr/dotnet/api/system.net.http.httpclient)
- [Documentation officielle de RestSharp](http://restsharp.org/)
- [Documentation officielle de Flurl](https://flurl.dev/)