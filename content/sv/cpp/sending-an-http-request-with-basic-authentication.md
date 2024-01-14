---
title:                "C++: Sända en http-begäran med grundläggande autentisering"
simple_title:         "Sända en http-begäran med grundläggande autentisering"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför
Om du vill utveckla en C++-applikation som kan hämta data från en server, är det viktigt att förstå hur man skickar HTTP-förfrågningar med grundläggande autentisering. Detta är en viktig del av många webbapplikationer och API:er som kräver autentisering för att få åtkomst till deras data. I denna bloggpost kommer vi att utforska hur man skickar en HTTP-förfrågan med grundläggande autentisering i C++.

## Så här gör du
För att skicka en HTTP-förfrågan med grundläggande autentisering i C++, behöver vi först inkludera nödvändiga bibliotek och deklarera variabler för vår förfrågan. Sedan använder vi en byggare för att bygga vår HTTP-förfrågan och lägger till autentiseringsuppgifterna. Här är ett exempel på hur det kan se ut i kod:

```C++
// Inkludera nödvändiga bibliotek
#include <iostream>
#include <curl/curl.h>

using namespace std;

int main() {

// Definiera variabler för vår HTTP-förfrågan
CURL *curl;
CURLcode res;
struct curl_slist *headers = nullptr;
string endpoint = "https://exempel.com/api/";
string username = "användarnamn";
string password = "lösenord";

// Skapa vår byggare och lägg till autentiseringsuppgifter
curl = curl_easy_init();
if (curl) {
  headers = curl_slist_append(headers, "Content-Type: application/json"); // Ange en valfri HTTP-header
  curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
  curl_easy_setopt(curl, CURLOPT_USERPWD, (username + ":" + password).c_str()); // Lägg till autentiseringsuppgifter
  curl_easy_setopt(curl, CURLOPT_URL, endpoint.c_str()); // Ange slutpunkten för HTTP-förfrågan
  res = curl_easy_perform(curl); // Utför vår HTTP-förfrågan
  if (res != CURLE_OK) { // Kontrollera om HTTP-förfrågan lyckades
    cout << "Något gick fel. Felkod: " << res << endl;
  }
  curl_easy_cleanup(curl); // Stäng vår byggare
  curl_global_cleanup(); // Stäng all curl-funktionalitet
}
return 0;
}
```

Om vår förfrågan lyckas, kommer vi att få en 200 OK-status och den efterfrågade datan i retur. Om det finns några problem med autentiseringen, kommer vi att få en 401 Felaktig autentisering-status.

## Djupdykning
När vi skickar en HTTP-förfrågan med grundläggande autentisering, krypteras våra autentiseringsuppgifter och skickas som en bas64-kodad sträng i en HTTP-header som heter "Authorization". Detta ger en grundläggande säkerhetsnivå när vi skickar känsliga uppgifter över internet.

Vi kan också lägga till fler headers, till exempel "Accept" för att ange vilken typ av data vi förväntar oss att få tillbaka och "Content-Type" för att ange vilken typ av data vi skickar.

## Se också
- [Curl - officiell dokumentation](https://curl.se/libcurl/c/http-authentication.html)
- [C++ - officiell dokumentation](https://isocpp.org/wiki/faq/basic-serialization#serialize-xml-portable)
- [Bas64-kodning - Wikipedia](https://sv.wikipedia.org/wiki/Base64)

## Se även
- [Curl - officiell dokumentation](https://curl.se/libcurl/c/http-authentication.html)
- [C++ - officiell dokumentation](https://isocpp.org/wiki/faq/basic-serialization#serialize-xml-portable)
- [Bas64-kodning - Wikipedia](https://sv.wikipedia.org/wiki/Base64)