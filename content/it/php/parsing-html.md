---
title:                "Analisi dei linguaggi html"
html_title:           "PHP: Analisi dei linguaggi html"
simple_title:         "Analisi dei linguaggi html"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/parsing-html.md"
---

{{< edit_this_page >}}

## Perché
Internet è diventata una fonte inesauribile di informazioni e molte volte è necessario estrarre dati specifici da pagine web. Il parsing HTML è uno strumento essenziale per ottenere informazioni strutturate e utili da queste pagine.

## Come fare
Lo strumento principale per il parsing HTML in PHP è la libreria DOM. Per iniziare, bisogna creare un documento DOM con la funzione `domdocument()` e caricarci il codice HTML da analizzare. Ad esempio:

```PHP
$dom = new domdocument();
$dom->loadHTML($html);
```
Dove `$html` è una stringa contenente il codice HTML da analizzare. Per ottenere le informazioni desiderate, è possibile usare metodi come `getElementsByTagName()` e `getAttribute()`. Ad esempio:

```PHP
$titoli = $dom->getElementsByTagName('h1'); //ottiene gli elementi <h1>
foreach ($titoli as $titolo) {
    echo $titolo->getAttribute('id'); //stampa l'attributo "id" dei titoli
}
```
L'output di questo codice sarà una lista di id dei titoli presenti nella pagina HTML.

## Deep Dive
Il parsing HTML è un argomento vasto e complesso che richiede una buona conoscenza del linguaggio e delle tecniche di manipolazione dei dati. È importante scegliere metodi efficienti e ottimizzare il codice per evitare errori e prestazioni lente. Inoltre, è consigliato utilizzare librerie esterne per semplificare il processo di parsing e sfruttare al massimo le potenzialità del PHP.

## Vedi anche
- [DomDocument PHP Manual](https://www.php.net/manual/en/class.domdocument.php)
- [Simple HTML DOM Parser](https://simplehtmldom.sourceforge.io/)