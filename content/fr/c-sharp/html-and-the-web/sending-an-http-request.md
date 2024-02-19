---
aliases:
- /fr/c-sharp/sending-an-http-request/
date: 2024-01-20 17:59:13.470592-07:00
description: "Envoyer une requ\xEAte HTTP, c'est demander des donn\xE9es \xE0 un serveur\
  \ web. On le fait pour interagir avec des services web, r\xE9cup\xE9rer des informations,\
  \ ou\u2026"
lastmod: 2024-02-18 23:09:08.824240
model: gpt-4-1106-preview
summary: "Envoyer une requ\xEAte HTTP, c'est demander des donn\xE9es \xE0 un serveur\
  \ web. On le fait pour interagir avec des services web, r\xE9cup\xE9rer des informations,\
  \ ou\u2026"
title: "Envoi d'une requ\xEAte HTTP"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Envoyer une requête HTTP, c'est demander des données à un serveur web. On le fait pour interagir avec des services web, récupérer des informations, ou communiquer entre applications.

## Comment Faire :

Voici comment envoyer une requête GET simple en C# en utilisant `HttpClient` :

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main(string[] args)
    {
        using (HttpClient client = new HttpClient())
        {
            HttpResponseMessage response = await client.GetAsync("http://example.com");
            if (response.IsSuccessStatusCode)
            {
                string responseData = await response.Content.ReadAsStringAsync();
                Console.WriteLine(responseData);
            }
            else
            {
                Console.WriteLine($"Erreur: {response.StatusCode}");
            }
        }
    }
}
```

Sortie éventuelle :

```
<!doctype html>
...
</html>
```

## Plongée Profonde :

Historiquement, on utilisait `WebRequest` en C#. Mais `HttpClient` est le choix moderne, optimisé pour les appels asynchrones et les connexions réutilisables. Pour choisir l'approche, considérez les performances, la simplicité, et si vous avez besoin de gérer des contextes d'appels complexes comme des cookies ou authentification.

D'autres options ? Oui, on peut utiliser `RestSharp` ou `Flurl` pour des fonctionnalités supplémentaires. En termes d'implémentation, notons que `HttpClient` est implémenté pour travailler avec les cas async/await, ce qui est idéal pour les interfaces utilisateur réactives et les services backend non bloquants.

## À Voir Aussi :

- [Documentation de `HttpClient`](https://docs.microsoft.com/fr-fr/dotnet/api/system.net.http.httpclient?view=net-6.0)
- [Comparaison entre `HttpClient` et `WebRequest`](https://docs.microsoft.com/fr-fr/dotnet/api/system.net.httpwebrequest?view=net-6.0)
- [Guide pour `RestSharp`](https://restsharp.dev/)
- [Guide pour `Flurl`](https://flurl.dev/)
