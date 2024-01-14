---
title:                "C#: Skicka ett http-begäran med grundläggande autentisering"
simple_title:         "Skicka ett http-begäran med grundläggande autentisering"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför

Att skicka HTTP-förfrågningar med grundläggande autentisering (basic authentication) är en vanlig process inom webbutveckling och kan vara användbart för att skydda känslig information eller begränsa åtkomst till vissa resurser. Genom att sätta grundläggande autentisering kan bara användare med korrekta autentiseringsuppgifter få åtkomst till en resurs.

## Så här gör du

För att skicka en HTTP-förfrågning med grundläggande autentisering i C# behöver du först importera "System.Net.Http" namespace. Sedan kan du använda "HttpClient" för att skicka en GET-förfrågan till en viss URL med autentiseringsuppgifterna i header-objektet.

```C#
using System.Net.Http;

var client = new HttpClient();

// Skapa en autentiseringssträng
var authString = string.Format("{0}:{1}", "användarnamn", "lösenord");
var base64 = Convert.ToBase64String(Encoding.ASCII.GetBytes(authString));
var authHeader = new AuthenticationHeaderValue("Basic", base64);

// Lägg till autentiseringssträngen i HTTP-förfrågningens header
client.DefaultRequestHeaders.Authorization = authHeader;

// Skicka en GET-förtrågan till en URL
var response = await client.GetAsync("https://www.example.com");

// Skriv ut svaret
Console.WriteLine(response.StatusCode);
Console.WriteLine(response.Content.ReadAsStringAsync().Result);
```

Detta kommer att skicka en GET-förfrågning till "https://www.example.com" med autentiserinsuppgifterna i header-objektet och skriva ut svaret i konsolen. Om autentiseringsuppgifterna är korrekta kommer du att se svaret från servern, annars kommer du att få ett felmeddelande.

## Fördjupning

När du skickar en HTTP-förfrågning med grundläggande autentisering, lägger du till en "Authorization" header i förfrågningen. Denna header innehåller autentiseringsuppgifterna som är kodade med Base64. Detta är en enkel form av autentisering och bör endast användas om förtroende för servern är högt.

För att säkerställa en säker autentisering bör du använda en annan typ av autentisering, såsom OAuth eller JWT. Du kan också kryptera autentiseringsuppgifterna med SSL/TLS för att skydda dem mot skadliga avlyssnare.

## Se även

[Förstå enkeld autentisering i HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)

[Autentisering med Basic- och Digest-metoder i .NET](https://docs.microsoft.com/en-us/dotnet/framework/network-programming/how-to-provide-basic-authentication-with-network-credentials)

[OAuth-autentisering i .NET](https://docs.microsoft.com/en-us/aspnet/core/security/authentication/oauth)