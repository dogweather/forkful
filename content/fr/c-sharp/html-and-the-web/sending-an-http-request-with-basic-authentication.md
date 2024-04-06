---
date: 2024-01-20 18:01:13.989585-07:00
description: "How to: L'authentification de base HTTP est une m\xE9thode standard\
  \ de l'Internet vieille de plusieurs d\xE9cennies. Bien qu'elle soit simple \xE0\
  \ impl\xE9menter,\u2026"
lastmod: '2024-04-05T21:53:59.273149-06:00'
model: gpt-4-1106-preview
summary: "L'authentification de base HTTP est une m\xE9thode standard de l'Internet\
  \ vieille de plusieurs d\xE9cennies."
title: "Envoi d'une requ\xEAte HTTP avec authentification de base"
weight: 45
---

## How to:
```C#
using System;
using System.Net;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;

public class BasicAuthExample
{
    private static async Task Main()
    {
        var url = "https://example.com/api/data";
        var username = "user";
        var password = "pass";
        var base64String = Convert.ToBase64String(Encoding.ASCII.GetBytes($"{username}:{password}"));

        using (var client = new HttpClient())
        {
            client.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue("Basic", base64String);

            HttpResponseMessage response = await client.GetAsync(url);
            if (response.IsSuccessStatusCode)
            {
                string responseBody = await response.Content.ReadAsStringAsync();
                Console.WriteLine(responseBody);
            }
            else
            {
                Console.WriteLine("Error: " + response.StatusCode);
            }
        }
    }
}
```
Output:
```
{"data":"Your requested data here..."}
```
Ou pour une erreur HTTP :
```
Error: Unauthorized
```

## Deep Dive
L'authentification de base HTTP est une méthode standard de l'Internet vieille de plusieurs décennies. Bien qu'elle soit simple à implémenter, elle n'est pas la plus sécurisée. L'identifiant et le mot de passe ne sont que faiblement masqués en base-64, mais pas chiffrés. Si l'on utilise cette méthode, HTTPS est obligatoire pour une meilleure sécurité.

Des alternatives plus sécurisées existent, comme OAuth et JWT (JSON Web Tokens), qui permettent une authentification plus robuste sans exposer directement les identifiants.

Techniquement, pour implémenter l'authentification de base en C#, il suffit de construire une chaîne de caractères contenant l'utilisateur et le mot de passe séparés par un deux-points, la coder en base-64 et la placer dans l'en-tête `Authorization` de la requête HTTP avec le préfixe "Basic".

## See Also
- [HttpClient Class in C#](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [Basic access authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [The Authorization Header](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)
- [HTTPS](https://en.wikipedia.org/wiki/HTTPS)
- [OAuth](https://oauth.net/)
- [JWT (JSON Web Tokens)](https://jwt.io/)
