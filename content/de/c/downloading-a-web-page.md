---
title:                "Herunterladen einer Webseite"
html_title:           "C: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Was & Warum?

Das Herunterladen einer Webseite bezieht sich auf den Prozess des Abrufs und Auslesens von Daten aus dem Internet. Programmierer nutzen dieses Verfahren, um Informationen von Websites zu erfassen und zu verarbeiten, um sie für verschiedene Anwendungen zu nutzen. Beispielsweise kann es verwendet werden, um Daten aus einer Webanwendung zu sammeln oder um automatisierte Aufgaben auszuführen, wie das Überprüfen von Aktienkursen oder das Extrahieren von Informationen für Suchmaschinen.

# Wie geht's?

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;
 
  curl = curl_easy_init();
  if(curl) {
    // URL der zu ladenden Webseite angeben
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");
    
    // Falls die Webseite eine HTTPS Verbindung verwendet, muss SSL aktiviert werden
    curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 1L);
    
    // Die zurückgegebenen Daten werden in die Standardausgabe geschrieben
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, fwrite);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, stdout);
    
    // Die Anfrage ausführen
    res = curl_easy_perform(curl);
    
    // Falls ein Fehler auftritt, wird eine Fehlermeldung ausgegeben
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
    
    // Ressourcen freigeben
    curl_easy_cleanup(curl);
  }
  
  return 0;
}
```

Beispiel Ausgabe:

```
<!doctype html>
<html>
<head>
   <title>Example Domain</title>
   
   <meta charset="utf-8" />
   <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
   <meta name="viewport" content="width=device-width, initial-scale=1" />
   <style type="text/css">
   body {
      background-color: #f0f0f2;
      margin: 0;
      padding: 0;
      font-family: -apple-system, system-ui, BlinkMacSystemFont, "Segoe UI", "Roboto", "Oxygen", "Ubuntu", "Cantarell", "Fira Sans", "Droid Sans", "Helvetica Neue", sans-serif;
      -webkit-font-smoothing: antialiased;
      font-size: 4vmin;
   }
   …
</head>
<body>
<div>
   <h1>Example Domain</h1>
   <p>This domain is established to be used for illustrative examples in documents. You may use this domain in examples without prior coordination or asking for permission.</p>
   <p><a href="http://www.iana.org/domains/example">More information...</a></p>
</div>
</body>
</html>
```

# Tiefer eintauchen

Das Herunterladen von Webseiten hat in den letzten Jahren an Bedeutung gewonnen, da immer mehr Anwendungen Informationen aus dem Internet benötigen. Eine alternative Methode zum Herunterladen von Webseiten ist das Scraping, bei dem auch Informationen von den Webseiten extrahiert werden können. Um das Herunterladen von Webseiten in C zu implementieren, wird häufig die cURL Bibliothek verwendet, die eine Vielzahl von Funktionen bietet, um mit dem Internet zu interagieren.

# Siehe auch

Offizielle Dokumentation der cURL Bibliothek: https://curl.haxx.se/libcurl/c/