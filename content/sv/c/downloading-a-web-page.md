---
title:                "Ladda ner en webbsida"
html_title:           "C: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Vad & Varför?

Att ladda ner en webbsida innebär att hämta all källkod, bilder och annat innehåll från en specifik webbadress och spara det på din dator. Det kan vara användbart för programerare som vill analysera en webbsida eller integrera dess innehåll i sitt eget program.

# Hur man gör:

Ett enkelt sätt att ladda ner en webbsida är att använda ett HTTP-bibliotek som cURL. Här är ett exempel på hur man laddar ner "www.example.com" och sparar innehållet i en textfil:

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
  CURL *curl;
  FILE *fp;
  CURLcode res;
  
  curl = curl_easy_init();
  
  if (curl) {
    fp = fopen("example.html", "wb");
    
    curl_easy_setopt(curl, CURLOPT_URL, "www.example.com");
    
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, NULL);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
    
    res = curl_easy_perform(curl);
    curl_easy_cleanup(curl);
    
    fclose(fp);
  }
  
  return 0;
}
```

Detta skapar en fil med namnet "example.html" som innehåller allt innehåll från webbsidan. Detta är en enkel grund som kan anpassas och utvecklas för mer avancerad användning.

# Djupdykning:

Ladda ner en webbsida är ett vanligt verktyg för automatisering och dataextraktion. Det kan också vara användbart för att testa webbapplikationer och utveckla webbintegrering i program. Istället för att använda cURL kan man också använda inbyggda funktioner i C, såsom "fopen ()" och "fputs ()". Dessa ger mer kontroll över hur data ska behandlas. Det finns också andra språk och program som specialiserat sig på att ladda ner och behandla webbinnehåll.

# Se också:

- [libcurl documentation](https://curl.haxx.se/libcurl/)
- [Guide to web page downloading in C](https://krazydad.com/tutorials/curl/)
- [Other approaches to downloading web pages](https://stackoverflow.com/questions/230853/how-to-download-a-url/230906)