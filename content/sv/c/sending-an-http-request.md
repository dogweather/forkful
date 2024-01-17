---
title:                "Skicka en HTTP-begäran"
html_title:           "C: Skicka en HTTP-begäran"
simple_title:         "Skicka en HTTP-begäran"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?
När en programmerare "skickar en HTTP-förfrågan" så betyder det helt enkelt att de ber en dator någon annanstans om information. Detta är en kritisk del av många webbapplikationer, eftersom den tillåter kommunikation mellan olika system och användare. 

 ## Så här: 
Här är ett exempel på hur en HTTP-förfrågan kan se ut i C-programmeringsspråket: 
```C
#include <stdio.h>

int main()
{
    printf("Skickar en HTTP-förfrågan.\n");
    return 0;
}
```

Output: 
``` 
Skickar en HTTP-förfrågan. 
```

## Mer ingående:
HTTP (Hypertext Transfer Protocol) används för att överföra information över nätverk, särskilt på internet. Det är grundläggande för att webbsidor och webbapplikationer ska fungera korrekt. Det finns olika sätt att skicka en HTTP-förfrågan, till exempel med hjälp av bibliotek som cURL eller TCP sockets. Men oavsett metod, så är målet att skicka en förfrågan och få tillbaka en respons med önskad information. 

## Se även:
För mer information om HTTP och hur det används i C-programmering, rekommenderar vi: [HTTP in C](https://www.geeksforgeeks.org/http-requests-in-c-language/).