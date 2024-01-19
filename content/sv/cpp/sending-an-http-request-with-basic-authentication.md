---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Elixir: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att sända en HTTP-begäran med grundläggande autentisering är något av en standardmetod för att skydda dina API-förfrågningar. Programmerare gör detta för att tillhandahålla en säker, autentiserad kommunikation mellan klient och server.

## Hur ska man:

I det här avsnittet kommer vi att använda den tredjepart-klassen cURLpp. Se till att du har lagt till literaller för ditt användarnamn och lösenord.

```C++
#include <curlpp/cURLpp.hpp>
#include <curlpp/Options.hpp>

int main() {
  curlpp::Cleanup myCleanup;

  try {
    curlpp::Easy request;
    request.setOpt<curlpp::options::Url>("http://example.com");
    request.setOpt<curlpp::options::HttpAuth>(CURLAUTH_BASIC);
    request.setOpt<curlpp::options::UserPwd>("username:password");

    request.perform();
  }
  catch ( curlpp::RuntimeError &e ) {
    std::cout << e.what() << std::endl;
  }
  catch ( curlpp::LogicError &e ) {
    std::cout << e.what() << std::endl;
  }

  return 0;
}
```

## Djupt Dyk

HTTP-begäran med grundläggande autentisering har varit grunden för webbsäkerhet sedan dess början. Det finns alternativ till grundläggande autentisering, till exempel OAuth och Token-baserad autentisering, men dessa kan vara mer komplexa att implementera och kan komma med extra säkerhetsrisker.

När det gäller implementeringsdetaljer för grundläggande autentisering i C++, är det viktigt att förstå att de autentiseringsuppgifter du anger (ditt användarnamn och lösenord) skickas i klartextformat. Detta innebär att de kan läsas av någon som lyssnar på nätverket, vilket gör det extra viktigt att använda säkra anslutningar som HTTPS.

## Se Även:

För mer information om HTTP-begäran med grundläggande autentisering, se följande länkar:

- cURLpp dokumentation: http://www.curlpp.org/
- HTTP Autentisering på MDN: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Säkerhetspraxis för Autentisering: https://www.owasp.org/index.php/Authentication_Cheat_Sheet