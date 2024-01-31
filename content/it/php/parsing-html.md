---
title:                "Analisi dell'HTML"
date:                  2024-01-20T15:33:00.019184-07:00
html_title:           "Bash: Analisi dell'HTML"
simple_title:         "Analisi dell'HTML"

category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/parsing-html.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
L'analisi (parsing) dell'HTML è il processo di scorrere ed estrarre dati da file HTML. I programmatori lo fanno per automazione, per recuperare informazioni da siti web o per manipolare documenti HTML in maniera programmatica.

## Come Fare:
```PHP
<?php
$doc = new DOMDocument();
@$doc->loadHTML(file_get_contents('https://www.esempio.com/'));
$xpath = new DOMXPath($doc);

// Estrarre tutti i link
foreach ($xpath->query('//a') as $anchor) {
    echo $anchor->getAttribute('href') . "\n";
}

// Ottenere il titolo di una pagina
$title = $xpath->query('//title')->item(0)->nodeValue;
echo $title . "\n";
?>
```
Output:
```
https://www.link1.com/
https://www.link2.com/
Titolo della Pagina Esempio
```

## Approfondimento
Historicamente, l'HTML è stato analizzato con espressioni regolari o parsing manuale - entrambi soggetti ad errori. Adesso, con DOM e XPath, abbiamo strumenti potenti e precisi. Alternativamente, ci sono librerie come SimpleHTMLDom o framework come Symfony’s DomCrawler. Implementare l'analisi in PHP inizia con la creazione di un DOMDocument e poi si manipola attraverso DOMXPath per queries specifiche.

## Altre Risorse
- Documentazione PHP su DOM: https://www.php.net/manual/it/book.dom.php
- Documentazione PHP su XPath: https://www.php.net/manual/it/book.domxpath.php
- GitHub di SimpleHTMLDom: https://github.com/simplehtmldom/simplehtmldom
- Symfony DomCrawler: https://symfony.com/doc/current/components/dom_crawler.html
