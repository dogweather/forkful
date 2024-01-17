---
title:                "Sända en http-begäran med grundläggande autentisering"
html_title:           "Bash: Sända en http-begäran med grundläggande autentisering"
simple_title:         "Sända en http-begäran med grundläggande autentisering"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför? 
Att skicka en HTTP-förfrågan med grundläggande autentisering är en metod som används av programmerare för att säkert utbyta data mellan en klient och en server. Genom att använda autentiseringsuppgifter i förfrågan kan man bekräfta identiteten på den som begär informationen, vilket är särskilt viktigt när det rör sig om känslig data.

## Så här gör du:
För att skicka en HTTP-förfrågan med grundläggande autentisering i Bash, används kommandot `curl` tillsammans med flaggan `--user` för att ange autentiseringsuppgifterna. Exempelvis:
```
curl --user username:password http://www.example.com
```

Output: Du får en respons från servern med det efterfrågade innehållet.

## Djupdykning:
För att förstå bakgrunden till varför grundläggande autentisering används inom HTTP, kan det vara bra att känna till att en av de tidigaste versionerna av HTTP 1.0 saknade inbyggd säkerhet. Genom att använda autentiseringsuppgifter i förfrågan, blir kommunikationen ändå säker och informationen kan utbytas utan risk för obehörig åtkomst. Det finns även andra metoder för autentisering som används inom HTTP, såsom OAuth, men grundläggande autentisering är fortfarande en vanlig och enkel metod att implementera.

## Se även:
Om du vill lära dig mer om Bash-kommandot `curl` och dess olika användningsområden kan du besöka [dess officiella dokumentation](https://curl.se/docs/manpage.html). Du kan även läsa mer om HTTP och dess säkerhetsaspekter på [Mozilla Developers](https://developer.mozilla.org/en-US/docs/Web/HTTP) eller [W3C](https://www.w3.org/Protocols/).