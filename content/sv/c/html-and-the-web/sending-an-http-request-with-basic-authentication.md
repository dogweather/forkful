---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:19.741445-07:00
description: "Att skicka en HTTP-beg\xE4ran med grundl\xE4ggande autentisering i C\
  \ inneb\xE4r att man skapar en HTTP-beg\xE4ran som inkluderar en autentiseringsheader\
  \ med\u2026"
lastmod: '2024-03-13T22:44:38.382265-06:00'
model: gpt-4-0125-preview
summary: "Att skicka en HTTP-beg\xE4ran med grundl\xE4ggande autentisering i C inneb\xE4\
  r att man skapar en HTTP-beg\xE4ran som inkluderar en autentiseringsheader med anv\xE4\
  ndaruppgifter kodade i Base64."
title: "S\xE4nda en HTTP-f\xF6rfr\xE5gan med grundl\xE4ggande autentisering"
weight: 45
---

## Vad och varför?
Att skicka en HTTP-begäran med grundläggande autentisering i C innebär att man skapar en HTTP-begäran som inkluderar en autentiseringsheader med användaruppgifter kodade i Base64. Detta är en vanlig metod för att lägga till ett enkelt autentiseringsskikt till HTTP-begäranden, vilket möjliggör programmatisk åtkomst till begränsade resurser.

## Hur:
För att skicka en HTTP-begäran med grundläggande autentisering i C behöver vi använda libcurl-biblioteket, ett populärt, mångsidigt och lättanvänt klient-sida URL-överföringsbibliotek. Det hanterar olika protokoll, inklusive HTTP och HTTPS, vilket gör vår uppgift enklare. Se till att libcurl är installerat på ditt system innan du fortsätter. Här är ett grundläggande exempel som visar hur man skickar en GET-begäran med grundläggande autentisering:

```c
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_DEFAULT);

    curl = curl_easy_init();
    if(curl) {
        // URL:en dit begäran skickas
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/resource");
        // Aktiverar användningen av grundläggande autentisering
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
        // Anger användarnamn och lösenord för den grundläggande autentiseringen
        curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");

        // Utför GET-begäran
        res = curl_easy_perform(curl);

        // Kontrollerar efter fel
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() misslyckades: %s\n",
                    curl_easy_strerror(res));

        // Städa alltid upp
        curl_easy_cleanup(curl);
    }
    
    curl_global_cleanup();

    return 0;
}
```
I exemplet ovan, ersätt `"http://example.com/resource"`, `"username"`, och `"password"` med din faktiska URL, användarnamn och lösenord.

Denna kod initierar ett `CURL`-objekt, ställer in URL:en, aktiverar HTTP Basic Authentication och specificerar referenserna. Därefter skickas begäran och städar upp efter sig. Om det lyckas hämtas den efterfrågade resursen; om det finns ett fel, skrivs det ut till stderr.

Exempelutdata (med antagandet om lyckad autentisering och åtkomst till resurser) kanske inte visas direkt av programmet, eftersom exemplet främst demonstrerar sändningen av begäran. För att skriva ut svaret skulle du behöva utöka programmet för att hantera HTTP-respondsdata.

## Fördjupning:
Att skicka HTTP-begäranden med grundläggande autentisering i C, som visat, utnyttjar libcurl-biblioteket för dess robusthet och enkelhet. Traditionellt sett var det omständigt och felbenäget att skapa HTTP-begäranden rent i C utan sådana bibliotek, vilket involverade programmering på lägre nivå med sockets och manuell konstruktion av HTTP-huvuden.

Grundläggande autentisering i sig är en metod från webbens tidiga dagar. Den skickar referenser i ett lätt dekodningsbart format (Base64), vilket är inneboende osäkert över okrypterade kanaler. Moderna applikationer föredrar ofta säkrare autentiseringsmetoder, såsom OAuth 2.0 eller JWT (JSON Web Tokens), särskilt för känsliga data.

Dock, för interna, mindre kritiska system, eller snabba-och-smutsiga skript där bekvämligheten väger tungare än säkerhetsbekymmer, förblir grundläggande autentisering i bruk. Dessutom, när det kombineras med krypterade anslutningar (HTTPS), blir dess enkelhet en fördel för snabb utveckling, testning eller automationsarbete där högre säkerhetsmekanismer inte är lika nödvändiga.

I sammanhang där absolut senaste inom säkerhet är oeftergivlig, bör alternativ som tokenbaserad autentisering prioriteras. Trots detta ger förståelsen i hur man implementerar grundläggande autentisering i C genom libcurl en grundläggande färdighet som kan anpassas till olika autentiseringsmetoder och protokoll, vilket återspeglar de nyanserade avvägningarna mellan säkerhet, bekvämlighet och applikationskrav i webbutveckling.
