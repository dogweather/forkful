---
date: 2024-01-20 17:43:37.759978-07:00
description: "How to: Pour t\xE9l\xE9charger le contenu d'une page web, on utilise\
  \ la classe `HttpClient`. Voici un exemple simple ."
lastmod: '2024-03-13T22:44:57.785751-06:00'
model: gpt-4-1106-preview
summary: "Pour t\xE9l\xE9charger le contenu d'une page web, on utilise la classe `HttpClient`."
title: "T\xE9l\xE9chargement d'une page web"
weight: 42
---

## How to:
Pour télécharger le contenu d'une page web, on utilise la classe `HttpClient`. Voici un exemple simple :

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main()
    {
        var url = "http://example.com";
        using var httpClient = new HttpClient();
        
        try
        {
            string content = await httpClient.GetStringAsync(url);
            Console.WriteLine(content);
        }
        catch (HttpRequestException e)
        {
            Console.WriteLine("Erreur lors du téléchargement de la page : {0} ", e.Message);
        }
    }
}
```

Sortie possible :
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
</html>
```

## Deep Dive
Historiquement, on utilisait `WebClient` ou `HttpWebRequest` pour télécharger du contenu web en C#. Mais `HttpClient`, introduit avec .NET 4.5, est désormais le choix préféré grâce à son interface moderne et sa gestion améliorée des connexions HTTP.

Parmi les alternatives, on retrouve des bibliothèques comme `RestSharp` ou `Flurl`, qui offrent des fonctionnalités supplémentaires pour les appels d'API REST.

L'implémentation d'un téléchargement propre sous-entend la gestion des exceptions, l'encodage correct, et potentiellement la manipulation de `HttpHeaders` pour se comporter comme un navigateur web classique.

## See Also
Pour approfondir:
- [HttpClient Class in MSDN](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [HttpClient vs WebClient vs HttpWebRequest](https://www.codeproject.com/Articles/1194406/HttpClient-vs-WebClient-vs-HttpWebRequest)
- [Introduction aux appels HTTP asynchrones en C#](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/async/)
