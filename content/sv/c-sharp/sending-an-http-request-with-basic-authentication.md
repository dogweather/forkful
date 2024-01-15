---
title:                "Sända en http-begäran med grundläggande autentisering"
html_title:           "C#: Sända en http-begäran med grundläggande autentisering"
simple_title:         "Sända en http-begäran med grundläggande autentisering"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför
Att skicka en HTTP-begäran med grundläggande autentisering är nödvändigt om man vill använda en webbtjänst som kräver autentisering. Det är en enkel och vanlig metod för att säkra åtkomst och identifiera användare.

## Så här
För att skicka en HTTP-begäran med grundläggande autentisering i C# behöver vi först skapa en instans av klassen `HttpClient`. Detta är ett inbyggt C#-verktyg som hjälper oss att hantera HTTP-begäran och svar. Sedan behöver vi definiera URL-en och lägga till vår autentiseringsinformation i vår begäran.

```C#
var client = new HttpClient();              // Skapar en instans av HttpClient
var url = "https://example.com/api/users";  // Definierar URL-en
client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue(    // Lägger till autentiseringsinformationen
    "Basic", Convert.ToBase64String(Encoding.ASCII.GetBytes("username:password")));   
```

I ovanstående kod måste vi ersätta "username" och "password" med våra egna autentiseringsuppgifter. Detta konverterar dem till Base64 och lägger till det i vår begäran som en authorization header. Nu kan vi skicka begäran genom att använda `client.GetAsync()` eller `client.PostAsync()`, beroende på vilken typ av begäran vi behöver skicka. Vi bör även hantera eventuella fel som kan uppstå genom att använda `try-catch`-block.

## Deep Dive
När vi skapar en instans av `HttpClient`-klassen, används en standardkonfiguration som inte tillåter grundläggande autentisering. Vi behöver därför specificera att vi vill använda det i vår begäran genom att ange det som en authorization header.

En annan viktig aspekt att tänka på är att autentiseringsuppgifterna måste krypteras innan de skickas. Detta görs genom att konvertera dem till Base64-kodning, vilket är ett sätt att representera binär data i textform.

En annan metod för autentisering som är vanligt förekommande är OAuth, som ofta används för API-autentiseringen. Detta är en mer säker metod som innebär att man använder en unik token för att autentisera istället för användarnamn och lösenord.

## Se även
- [Microsoft Docs - Skicka HTTP-begäran med grundläggande autentisering](https://docs.microsoft.com/sv-se/dotnet/csharp/programming-guide/concepts/linq/sending-a-basic-authentication-request)
- [MDN Web Docs - Autentisering i HTTP](https://developer.mozilla.org/sv-SE/docs/Web/HTTP/Authentication)