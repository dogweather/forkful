---
title:                "Analysering av html"
html_title:           "C#: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å parse HTML handler om å konvertere HTML-koden til et format en applikasjon lettere kan jobbe med (f.eks. et trestrukturert objekt). Programmerere gjør dette for å manipulere, hente data fra, eller generere HTML-dokumenter på en mer strukturert og grei måte.

## Hvordan:
Her er et grunnleggende eksempel på hvordan du bruker "htmlcxx" -biblioteket for å parse HTML i C++:
```C++
#include <iostream>
#include <htmlcxx/html/ParserDom.h>

int main() {
    std::string html = "<html><body><h1>Hei, Verden!</h1></body></html>";
    htmlcxx::HTML::ParserDom parser;
    tree<htmlcxx::HTML::Node> dom = parser.parseTree(html);

    // Utforsk DOM-treet:
    for(auto it = dom.begin(); it != dom.end(); ++it)
    {
        if(!it->isTag() && !it->isComment())
        {
            std::cout << it->text() << std::endl;
        }
    }
    return 0;
}
```
Når du kjører dette programmet, vil outputtet være: `Hei, Verden!`

## Dyp Dykk
1) Historisk kontekst: Parsing av HTML har vært nødvendig siden det første HTML-dokumentet ble opprettet. Den tidligste form av parsing var kanskje ved hjelp av regulære uttrykk, men dette er upraktisk og feilutsatt for mer komplekse dokumenter.

2) Alternativer: Det finnes mange bibliotek for parsing av HTML i C++. Noen av disse er "htmlcxx", "Gumbo" (utviklet av Google), og "MyHtml". Hver av disse har sine fordeler og ulemper og man bør velge den som best passer prosjektets behov.

3) Implementeringsdetaljer: Parsing av HTML går ut på å lese en HTML-tekst, forstå dens struktur, og deretter lage en representasjon av denne strukturen (oftest et DOM-tre). prosessen kan deles opp i to deler: lexical analyse (å bryte opp teksten i "tokens"), og syntaktisk analyse (å forstå hvordan disse "tokens" går sammen for å lage en struktur).

## Se Også
Her er noen lenker til biblioteker og ressurser som kan være hjelpsomme: 
1) htmlcxx: http://htmlcxx.sourceforge.net/
2) Gumbo: https://github.com/google/gumbo-parser
3) MyHtml: https://github.com/lexborisov/myhtml
4) W3C's HTML5 spesifikasjon (Detaljert info om HTML syntax): http://www.w3.org/TR/html5/syntax.html