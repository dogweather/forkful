---
title:                "Télécharger une page web"
html_title:           "Bash: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Télécharger une page web avec C#

## Quoi & Pourquoi ?

Télécharger une page web consiste à récupérer tout le contenu HTML d'un site web sur votre machine local. Les programmeurs font ça pour diverses raisons, par exemple, pour analyser les données d'un site Web ou pour créer des versions hors ligne des pages.

## Comment faire :

Voici une méthode simple pour télécharger une page web en utilisant C# avec HttpClient.

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main(string[] args)
    {
        await DownloadPageAsync();
    }

    static async Task DownloadPageAsync()
    {
        var url = "https://www.votresite.com";
        using (HttpClient client = new HttpClient())
        using (HttpResponseMessage response = await client.GetAsync(url))
        using (HttpContent content = response.Content)
        {
            string result = await content.ReadAsStringAsync();
            Console.WriteLine(result);
        }
    }
}
```

Lorsque vous exécutez ce programme, il téléchargera la page web et affichera le contenu HTML dans la console.

## Plongée en profondeur

Historiquement, les programmeurs utilisaient la classe `WebClient` pour télécharger des pages web en C#, mais l'équipe .NET recommande maintenant l'utilisation du `HttpClient`.

Il y a aussi d'autres alternatives comme `RestSharp` ou l'utilisation de librairies bas niveau comme `HttpWebRequest`.

Lorsque vous utilisez `HttpClient` pour télécharger une page Web, veillez à encapsuler le tout dans une instruction `using` pour vous assurer que les ressources réseau sont correctement libérées après l'utilisation.

## Voir aussi

1. Documentation .NET HttpClient : https://docs.microsoft.com/fr-fr/dotnet/api/system.net.http.httpclient
2. Téléchargement de fichiers avec C# : https://docs.microsoft.com/fr-fr/dotnet/csharp/programming-guide/concepts/async/how-to-download-a-file 
3. Guide RestSharp : https://restsharp.dev/getting-started/