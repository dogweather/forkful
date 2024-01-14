---
title:                "C#: Å sende en http forespørsel med grunnleggende autentisering"
simple_title:         "Å sende en http forespørsel med grunnleggende autentisering"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Hvorfor

HTTP-forespørsler med grunnleggende autentisering er en vanlig måte å sikre at en bruker har tilgang til en ressurs på nettet. Dette kan være nyttig for å beskytte følsomme data eller for å begrense tilgang til bestemte brukergrupper. I denne bloggposten vil vi gå gjennom hvordan du kan sende en HTTP-forespørsel med grunnleggende autentisering ved hjelp av C#.

# Hvordan

For å sende en HTTP-forespørsel med grunnleggende autentisering i C#, må vi først legge til en header med autentiseringsinformasjonen i forespørselen. Dette kan gjøres ved å bruke ```HttpClient```-klassen. Sånn ser det ut:

```C#
HttpClient client = new HttpClient();
var credentials = Encoding.ASCII.GetBytes("brukernavn:passord");
client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Basic", Convert.ToBase64String(credentials));
```

I dette eksempelet bruker vi en ```HttpClient```-instans for å sende en GET-forespørsel. Vi legger til en header med autentiseringsinformasjonen ved å konvertere brukernavn og passord til en Base64-streng og legge den til i Authorization-headeren.

Når dette er gjort, kan vi legge til url-adressen vi ønsker å sende en forespørsel til i en ```HttpRequestMessage```, og sende den til riktig ressurs ved å bruke ```client.SendAsync(request)```. Her er et fullstendig eksempel:

```C#
using System;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;

namespace HTTPBasicAuthentication
{
    class Program
    {
        static void Main(string[] args)
        {
            HttpClient client = new HttpClient();

            var credentials = Encoding.ASCII.GetBytes("brukernavn:passord");
            client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Basic", Convert.ToBase64String(credentials));

            HttpRequestMessage request = new HttpRequestMessage();
            request.RequestUri = new Uri("https://example.com");
            request.Method = HttpMethod.Get;

            var response = client.SendAsync(request).Result;
            var responseBody = response.Content.ReadAsStringAsync().Result;

            Console.WriteLine(responseBody);
        }
    }
}
```

Dette eksempelet vil sende en GET-forespørsel til ```https://example.com``` med autentiseringsheaderen lagt til.

# Detaljert gjennomgang

Nå som vi har sett et eksempel på hvordan vi kan sende en HTTP-forespørsel med grunnleggende autentisering, la oss gå litt dypere inn på hvordan autentiseringsprosessen fungerer.

Grunnleggende autentisering fungerer ved at brukernavnet og passordet blir kodet til en Base64-streng og lagt til i Authorization-headeren i en HTTP-forespørsel. Ved mottak av denne forespørselen, vil serveren lese headeren og sammenligne den med den lagrede autentiseringen for å avgjøre om brukeren har tilgang eller ikke.

Det er viktig å merke seg at grunnleggende autentisering ikke er en sikker autentiseringsmetode, da brukernavn og passord kan avlyttes gjennom nettverket. Derfor anbefales det å bruke HTTPS i kombinasjon med grunnleggende autentisering for å sikre en trygg kommunikasjon.

# Se også

- [Microsoft Docs: HTTP-forespørsler med grunnleggende autentisering i C#](https://docs.microsoft.com/nb-no/dotnet/api/system.net.http.httpclient?view=netcore-3.1)
- [Dagens Næringsliv: Sikkerhet på nettet med HTTPS](https://www.dn.no/kapital/datasikkerhet/nettsikkerhet/sikkerhet-pa-nettet/2-1-200798)