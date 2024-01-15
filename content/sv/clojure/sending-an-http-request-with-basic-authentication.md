---
title:                "Sända en http-begäran med grundläggande autentisering"
html_title:           "Clojure: Sända en http-begäran med grundläggande autentisering"
simple_title:         "Sända en http-begäran med grundläggande autentisering"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Varför: För att kunna kommunicera med en server och åtkomst till dess skyddade resurser, är det ofta nödvändigt att skicka en HTTP-begäran med grundläggande autentisering. Detta säkerställer att endast auktoriserade användare har tillgång till de resurser som skyddas av servern.

Hur man gör det: 
```Clojure
(require '[clj-http.client :as client])

(client/get "https://www.example.com/" {:basic-auth ["username" "password"]})
```
Detta är ett exempel på hur en HTTP-begäran med grundläggande autentisering kan skickas med hjälp av Clojure-biblioteket "clj-http". Funktionen "get" tar emot två argument - en URL och en karta av begärförbindelser. I detta fall har vi lagt till en begärförbindelse med nyckeln ":basic-auth" och värdena för användarnamn och lösenord i en vektor.

```Clojure
{:status 200
 :headers {"Content-Type" "text/html;charset=utf-8"}
 :body "<!DOCTYPE html><h1>Välkommen till exempel-webbplatsen</h1>"}
```
Efter att ha skickat begäran, kommer vi att få ett svar, representerat som en karta med tre nycklar - ":status" som visar statuskoden, ":headers" som innehåller information om svarsrubriker och ":body" som innehåller själva innehållet i svaret från servern. Här ser vi att statuskoden är 200 vilket betyder att begäran har lyckats och vi har tillgång till resursen.

Djupdykning: Vid grundläggande autentisering, skickar klienten sitt användarnamn och lösenord i klartext till servern. Detta kan skapa säkerhetsproblem eftersom informationen kan fångas upp av en angripare. Därför är det viktigt att använda HTTPS för att kryptera kommunikationen och skydda användarinformationen.

Se också:
- https://github.com/dakrone/clj-http - "clj-http" biblioteket
- https://www.w3.org/Protocols/rfc2616/rfc2616-sec11.html - HTTP-autentiseringsstandarder
- https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme - Mer information om HTTP autentisering