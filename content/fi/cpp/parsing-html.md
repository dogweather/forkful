---
title:                "HTML:n jäsentäminen"
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

HTML-koodin jäsennys tarkoittaa HTML-dokumentin rakenteellisen tiedon erottamista sen esitystavasta. Ohjelmoijat jäsentelevät HTML-dokumentteja, jotta he voivat käsitellä ja analysoida verkkosisältöä automaattisesti.

## Miten:

Esimerkkikoodi ja tulosteet:
```C++
#include <iostream>
#include <gumbo.h>

int main() {
    const char* html =
    "<html>\
        <body>\
            <h1>Hello, world!</h1>\
        </body> \
    </html\>";

    GumboOutput* output = gumbo_parse(html);
    std::cout << output->root->v.document.children.length << std::endl;

    gumbo_destroy_output(&kGumboDefaultOptions, output);
  
    return 0;
}
```

Esimerkkikoodimme tulosteeksi saadaan `2`, mikä tarkoittaa, että juurielementillä on kaksi lapsielementtiä.

## Syvällisempi sukellus:

HTML-jäsennys syntyi webin alkuvuosina, jolloin sisällön esitystapa määritti sen rakenteen. Nykypäivänä on olemassa monia vaihtoehtoisia jäsennyskeinoja, kuten SAX ja DOM.

C++ HTML-jäsennys käyttää tyypillisesti kirjastoja, kuten Gumbo, joka on esitelty esimerkissämme. Tieto pakataan sisäiseen metsänmuotoiseen datastruktuuriin ja sen jälkeen käsitellään.

## Katso myös:

1. W3C HTML5 - Lyhyt esittely: [linkki](https://www.w3.org/TR/html5/syntax.html#parsing)
2. Google Gumbo – GitHub: [linkki](https://github.com/google/gumbo-parser)
3. Sax vs. Dom: [linkki](http://stackoverflow.com/questions/6828703/difference-between-sax-and-dom)