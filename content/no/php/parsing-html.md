---
title:                "Analysere HTML."
html_title:           "PHP: Analysere HTML."
simple_title:         "Analysere HTML."
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/parsing-html.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du jobber med å utvikle nettsider eller applikasjoner, vil du sannsynligvis på et tidspunkt måtte hente eller manipulere data fra andre nettsteder. Ved å parse HTML, kan du enkelt trekke ut spesifikke deler av koden og bruke dem til å lage dynamiske og tilpassede løsninger for dine prosjekter.

## Hvordan gjøre det

Det finnes flere måter å parse HTML i PHP på, men det enkleste og mest effektive er å bruke et tredjeparts bibliotek som "simple_html_dom". For å bruke dette, må du først laste ned biblioteket fra nettet og lagre det i en mappe på serveren din.

Deretter kan du importere biblioteket og begynne å bruke det til å hente data fra en nettside. Her er et eksempel på hvordan du kan hente overskriftene fra en nettside og skrive dem ut:

```PHP
// Importer biblioteket
include "simple_html_dom.php";

// Hent nettsiden du vil parse
$html = file_get_html('https://www.example.com');

// Finn alle overskrifter på siden
foreach($html->find('h1, h2, h3') as $heading) {
  // Skriv ut overskriften
  echo $heading->innertext;
}
```

Dette eksempelet vil skrive ut alle overskriftene på nettsiden "www.example.com". Du kan også bruke forskjellige selektorer for å hente andre elementer, som linker, bilder og tabeller.

## Dypdykk

Når du parser HTML i PHP, er det viktig å forstå hvordan koden fungerer. Simple_html_dom-biblioteket bruker en prinsipp kalt "DOM", som står for Document Object Model. Dette vil si at HTML-koden blir representert som et hierarki av objekter, hvor hvert objekt tilsvarer et HTML-element.

Du kan også bruke CSS-selektorer i stedet for de vanlige HTML-taggene for å få mer presise resultater når du parser HTML. For eksempel kan du bruke ".class" for å hente alle elementer med den gitte klassen i stedet for å bruke "div" for alle div-elementer.

## Se også

Her er noen nyttige ressurser for å lære mer om parsing av HTML i PHP:

- [PHP Simple HTML DOM](https://simplehtmldom.sourceforge.io/) - offisiell dokumentasjon for biblioteket brukt i dette eksempelet.
- [W3Schools - PHP HTML DOM](https://www.w3schools.com/php/php_ref_simplexml.asp) - en god ressurs for å lære mer om DOM og hvordan det brukes i PHP.
- [PHP.net - DOMDocument](https://www.php.net/manual/en/class.domdocument.php) - dokumentasjon for PHPs innebygde DOM Document-klasser.