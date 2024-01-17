---
title:                "Envoyer une demande http"
html_title:           "C#: Envoyer une demande http"
simple_title:         "Envoyer une demande http"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

L'envoi de requêtes HTTP est un processus couramment utilisé par les programmeurs pour communiquer avec des serveurs web et récupérer des données. Cela peut être fait à l'aide de différentes méthodes, telles que GET, POST, PUT et DELETE, pour effectuer des actions spécifiques sur le serveur.

Les programmeurs envoient des requêtes HTTP pour interagir avec des API, récupérer des données pour les applications, ou pour effectuer des opérations telles que la mise à jour d'informations sur un serveur. C'est un moyen rapide et efficace d'obtenir des informations ou d'exécuter des actions sans avoir à accéder physiquement au serveur.

## Comment:

Pour envoyer une requête HTTP en utilisant C#, il est nécessaire d'utiliser la classe ```HttpClient``` du namespace ```System.Net.Http```. Voici un exemple de code pour effectuer une requête GET en utilisant cette classe:

```
using System;
using System.Net.Http;

public class Program
{
    public static async Task Main()
    {
        HttpClient client = new HttpClient();
        HttpResponseMessage response = await client.GetAsync("https://example.com/api/data");
        string result = await response.Content.ReadAsStringAsync();
        Console.WriteLine(result);
    }
}
```

Cela créera un objet ```HttpClient``` et l'utilisera pour effectuer une requête GET sur l'URL fournie. La réponse sera stockée dans un ```HttpResponseMessage``` et peut être lue en utilisant la méthode ```ReadAsStringAsync()```. Le résultat sera affiché dans la console.

## Profonde plongée:

L'envoi de requêtes HTTP est un concept qui existe depuis les premiers jours d'Internet. Cependant, avec l'avancement de la technologie, de nouvelles méthodes sont apparues, telles que les sockets TCP et UDP, pour communiquer avec les serveurs. Cependant, l'utilisation d'HTTP reste populaire en raison de sa simplicité et de sa capacité à interagir avec différentes plates-formes.

Il existe également d'autres bibliothèques que ```HttpClient``` pour envoyer des requêtes HTTP en C#. Parmi elles, on trouve ```WebClient```, ```WebRequest``` et ```RestSharp```. Chaque bibliothèque a ses propres avantages et inconvénients, et il est important de les comparer avant de choisir celle qui convient le mieux à votre projet.

En termes d'implémentation, l'envoi de requêtes HTTP en C# peut également inclure des configurations telles que l'utilisation de certificats SSL, l'ajout de paramètres ou de headers personnalisés, etc. Il peut également être utilisé en combinaison avec des frameworks tels que ASP.NET pour créer des applications web hautement évolutives.

## Voir aussi:

- [Microsoft Docs - Utilisation de HttpClient](https://docs.microsoft.com/fr-fr/dotnet/api/system.net.http.httpclient?view=netframework-4.8)
- [Comparaison entre HttpClient, Flurl et RestSharp](https://www.codingame.com/playgrounds/5126/comparaison-entre-httpclient-lurl-librairie-et-restsharp)
- [Understanding POST and GET Requests](https://www.thecodingguys.net/resources/understanding-get-and-post-requests)