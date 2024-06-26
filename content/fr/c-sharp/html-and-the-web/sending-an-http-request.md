---
date: 2024-01-20 17:59:13.470592-07:00
description: "Comment Faire : Voici comment envoyer une requ\xEAte GET simple en C#\
  \ en utilisant `HttpClient` ."
lastmod: '2024-04-05T21:53:59.269868-06:00'
model: gpt-4-1106-preview
summary: "Voici comment envoyer une requ\xEAte GET simple en C# en utilisant `HttpClient`\
  \ ."
title: "Envoi d'une requ\xEAte HTTP"
weight: 44
---

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
