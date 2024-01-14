---
title:                "Clojure: Sända en http-förfrågan med grundläggande autentisering"
simple_title:         "Sända en http-förfrågan med grundläggande autentisering"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Varför

Att skicka en HTTP-förfrågan med grundläggande autentisering är ofta nödvändigt för att få tillgång till skyddade resurser på en webbtjänst. Detta kan vara användbart för att hämta data från en API eller logga in på en webbplats med användarnamn och lösenord.

## Så här gör du

För att skicka en HTTP-förfrågan med grundläggande autentisering i Clojure krävs det att du använder en HTTP-bibliotek som stöder autentisering. Ett populärt alternativ är "clj-http".

Först måste du importera biblioteket genom att lägga till följande kod i din fil:

```Clojure
(ns min-projekt.core
  (:require [clj-http.client :as http]))
```

Sedan kan du använda funktionen "http/basic-auth" för att skapa autentiseringsuppgifter och lägga till dem i din HTTP-förfrågan. Till exempel:

```Clojure
(http/get "http://www.example.com" 
          {:basic-auth ["användarnamn" "lösenord"]})
```

Detta kommer att lägga till en Authorization-header i din förfrågan med autentiseringsuppgifterna.

## Djupdykning

När du skickar en HTTP-förfrågan med grundläggande autentisering, skickas autentiseringsuppgifterna i klartext över nätverket. Detta är inte en säker metod för autentisering och bör undvikas om möjligt.

Det finns också andra metoder för autentisering som kan användas istället för grundläggande autentisering, till exempel OAuth eller API-nycklar. Dessa metoder är mer säkra och bör övervägas när det är möjligt.

# Se även

- [clj-http](https://github.com/dakrone/clj-http)
- [OAuth i Clojure](https://github.com/mattrepl/clj-oauth)