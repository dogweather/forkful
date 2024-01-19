---
title:                "Scaricare una pagina web"
html_title:           "C++: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Cos'è & Perché? 
Scaricare una pagina web significa acquisire i dati di quella pagina nel tuo sistema locale. I programmatori lo fanno per analizzare, testare e manipolare i contenuti web per vari scopi, come lo scraping dei dati o il testing automatizzato.

## Come fare:
Scarichiamo una pagina web in C++ con la libreria `CPR`. Ecco come:

```C++
#include <cpr/cpr.h>

int main() {
    cpr::Response r = cpr::Get(cpr::Url{"http://example.com/"});
    // Il corpo della pagina web scaricata è accessibile con r.text
    std::cout << r.text << std::endl;
}
```

Ecco un esempio di output:

```C++
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## Approfondimento
Scaricare pagine web è una pratica che risale agli albori del web, quando i programmatori scrivevano script per navigare i primi siti web. Oggi ci sono molte alternative all'uso del C++, come Python o Node.js, che offrono librerie dedicate per il web scraping e il testing. 

Tuttavia, l'utilizzo di C++ presenta alcuni vantaggi. È una lingua potente e versatile, adatta per lavorare con applicazioni a basso livello o ad alte prestazioni. Inoltre, con librerie come `CPR`, l'implementazione del download di una pagina web può essere altrettanto semplice e diretta.

## Vedi Anche
1. [CPR - Curl for People, a spiritual port of Python Requests](https://github.com/whoshuu/cpr)
2. [Web Scraping with Python](https://realpython.com/python-web-scraping-practical-introduction/)
3. [Web Scraping with Node.js](https://www.freecodecamp.org/news/the-ultimate-guide-to-web-scraping-with-node-js-daa2027d5313/)