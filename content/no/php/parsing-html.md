---
title:                "Analysering av html"
html_title:           "PHP: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/parsing-html.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?

Parsing av HTML er prosessen med å lese og analysere HTML-koden i en nettside. Dette er et viktig verktøy for webutviklere for å hente ut spesifikke data eller informasjon fra en nettside. Det kan også brukes til å omforme HTML-koden for å tilpasse den til ulike formater eller til å lagre den for senere bruk.

# Hvordan:

For å utføre enkel HTML parsing i PHP kan du bruke funksjonene `file_get_contents()` og `strpos()`. Her er et eksempel på hvordan du kan hente ut tittel og beskrivelse fra en nettside:

```PHP
$url = "https://www.example.com";
$html = file_get_contents($url);

$title_start = strpos($html, '<title>') + 7;
$title_end = strpos($html, '</title>');
$title = substr($html, $title_start, $title_end - $title_start);

$desc_start = strpos($html, '<meta name="description" content="') + 34;
$desc_end = strpos($html, '"', $desc_start);
$desc = substr($html, $desc_start, $desc_end - $desc_start);

echo "Tittel: $title <br>";
echo "Beskrivelse: $desc";
```

Dette eksempelet viser hvordan du kan hente ut tittel og beskrivelse fra en nettside ved å finne start- og sluttpunktene for elementene i HTML-koden. Resultatet vil være:

```
Tittel: Nettside Tittel
Beskrivelse: Dette er en beskrivelse av nettsiden.
```

# Dypdykk:

HTML parsing har vært en viktig del av webutvikling siden begynnelsen av internettets levetid. Det finnes også alternativer til å bruke PHP for parsing, som for eksempel JavaScript eller tredjeparts biblioteker som Simple HTML DOM.

Det er viktig å være klar over at parsing av HTML kan være en kompleks oppgave, spesielt med tanke på at nettsider kan variere i oppbygning og struktur. Det er derfor viktig å ha god kjennskap til HTML og ulike parser-teknikker for å få best mulig resultat. 

# Se også:

- [PHP - file_get_contents()](https://www.php.net/manual/en/function.file-get-contents.php)
- [PHP - strpos()](https://www.php.net/manual/en/function.strpos.php)
- [Simple HTML DOM](https://simplehtmldom.sourceforge.io/)