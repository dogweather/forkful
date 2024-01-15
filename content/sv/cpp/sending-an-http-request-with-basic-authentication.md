---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "C++: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför
Om du vill skicka en HTTP förfrågan med grundläggande autentisering är det för att autentisera dig själv till en server. Detta är särskilt viktigt när du behöver komma åt skyddad information eller utföra åtgärder på en server.

## Hur man gör det
För att skicka en HTTP förfrågan med grundläggande autentisering behöver du följa några enkla steg:

1. Importera nödvändiga bibliotek. In C++ kan du använda `#include <curl/curl.h>` för att använda cURL biblioteket som hjälper dig att skicka HTTP förfrågningar.
2. Skapa en variabel för cURL handler med `CURL *curl;` och en variabel för att lagra autentiseringsinstruktionerna med `struct curl_slist *headers = NULL;`.
3. Ange URL-adressen för servern i en variabel, till exempel `const char *url = "http://www.example.com/"`.
4. Skapa en variabel för att lagra användarnamnet och lösenordet för autentisering, till exempel `const char *userpwd = "användarnamn:lösenord"`.
5. Initialisera cURL biblioteket med `curl_global_init(CURL_GLOBAL_ALL);` och cURL handler med `curl = curl_easy_init();`.
6. Ange autentiseringsinstruktionen i headern med `headers = curl_slist_append(headers, "Authorization: Basic " + userpwd);`.
7. Ange URL-adressen till cURL handle med `curl_easy_setopt(curl, CURLOPT_URL, url);`.
8. Ange headern till cURL handle med `curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);`.
9. Skicka den faktiska HTTP förfrågan med `curl_easy_perform(curl);`.
10. Städa upp genom att ta bort headern med `curl_slist_free_all(headers);` och avsluta cURL handler med `curl_easy_cleanup(curl);`.
11. Avsluta cURL biblioteket med `curl_global_cleanup();`.

Här är ett exempel på hur koden skulle kunna se ut:

```C++
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  struct curl_slist *headers = NULL;
  const char *url = "http://www.example.com/";
  const char *userpwd = "användarnamn:lösenord";

  curl_global_init(CURL_GLOBAL_ALL);
  curl = curl_easy_init();

  headers = curl_slist_append(headers, "Authorization: Basic " + userpwd);

  curl_easy_setopt(curl, CURLOPT_URL, url);
  curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);

  curl_easy_perform(curl);

  curl_slist_free_all(headers);
  curl_easy_cleanup(curl);
  curl_global_cleanup();

  return 0;
}
```

## Djupdykning
Vissa servrar kräver att du först måste autentisera dig själv för att få åtkomst till den faktiska resursen eller för att få tillgång till vissa funktioner. Det finns olika typer av autentisering, som baseras på användarnamn och lösenord, OAuth, JWT och mer. Med grundläggande autentisering skickas användarnamn och lösenord i klartext i headern för en HTTP förfrågan, vilket gör det mindre säkert jämfört med andra autentiseringsmetoder.

## Se även
- [cURL bibliotekets hemsida](https://curl.haxx.se/libcurl/)
- [Guide för HTTP autentisering i cURL](https://curl.haxx.se/libcurl/c/CURLOPT_HTTPAUTH.html)
- [HTTP autentisering på Wikipedia](https://en.wikipedia.org/wiki/Basic_access_authentication)