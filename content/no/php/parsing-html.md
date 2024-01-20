---
title:                "Analysering av html"
html_title:           "C#: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/parsing-html.md"
---

{{< edit_this_page >}}

# Å parse HTML med PHP

## Hva og hvorfor?

Å parse HTML er prosessen der vi dekoder HTML-koden for å forstå dens struktur og innhold. Programmerere gjør dette for å ekstrahere data, manipulere strukturen, eller integrere innhold fra nettsider.
  
## Slik gjør du

Vi bruker `DOMDocument`-klassen i PHP for å parse HTML. Først oppretter vi en ny instans av `DOMDocument`.

```PHP
$dom = new DOMDocument;
```

Deretter laster vi inn HTML-koden vi vil parse.

```PHP
$dom->loadHTML($html);
```

Vi kan nå få tilgang til HTML-elementene.

```PHP
$headings = $dom->getElementsByTagName('h1');
foreach ($headings as $heading) {
    echo $heading->nodeValue, PHP_EOL;
}
```
  
## Dypdykk

**Historisk kontekst**: PHP startet ut som en templating motor og har over tid utviklet seg til å bli en fullverdig programmeringsspråk med mange funksjoner, inkludert HTML-parsing.

**Alternativer**: Andre biblioteker som `phpQuery` og `simplehtmldom` gir mer brukervennlige grensesnitt for HTML parsing, men kan være overkill for enkle oppgaver.

**Implementeringsdetaljer**: `DOMDocument`-klassen bruker libxml2 til å parse HTML. libxml2 er en XML parser og toolkit skrevet i C for Gnome prosjektet.

## Se også

- [PHP DOMDocument Manual](https://www.php.net/manual/en/class.domdocument.php)
- [phpQuery on GitHub](https://github.com/phpquery/phpquery)
- [Simple HTML DOM Parser Manual](https://simplehtmldom.sourceforge.io/manual.htm)
- [libxml2](http://xmlsoft.org/)