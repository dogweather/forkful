---
date: 2024-01-20 18:01:13.989585-07:00
description: "Envoyer une requ\xEAte HTTP avec une authentification de base, c'est\
  \ envoyer un nom d'utilisateur et un mot de passe cod\xE9s en base-64 dans l'en-t\xEA\
  te de la\u2026"
lastmod: '2024-03-13T22:44:57.786908-06:00'
model: gpt-4-1106-preview
summary: "Envoyer une requ\xEAte HTTP avec une authentification de base, c'est envoyer\
  \ un nom d'utilisateur et un mot de passe cod\xE9s en base-64 dans l'en-t\xEAte\
  \ de la requ\xEAte."
title: "Envoi d'une requ\xEAte HTTP avec authentification de base"
weight: 45
---

## What & Why?
Envoyer une requête HTTP avec une authentification de base, c'est envoyer un nom d'utilisateur et un mot de passe codés en base-64 dans l'en-tête de la requête. Les programmeurs font cela pour accéder à des API ou des services web qui exigent une identification simple et rapide.

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
