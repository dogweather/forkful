---
title:                "Analyse syntaxique de HTML"
date:                  2024-01-20T15:32:17.861310-07:00
html_title:           "Arduino: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"

category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/parsing-html.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
L'analyse de HTML, c'est quand on lit et on interprète le code HTML pour extraire des données ou comprendre la structure d'une page web. Pourquoi ? Pour automatiser des tâches comme scraper des sites, ou pour manipuler le contenu directement dans nos applications.

## Comment faire :
Voici un petit bout de code avec Jsoup, une bibliothèque Java populaire pour parser le HTML.

```java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

public class HtmlParserDemo {
    public static void main(String[] args) {
        String html = "<html><head><title>Exemple</title></head>"
                    + "<body><p>Ceci est un paragraphe.</p></body></html>";
        Document doc = Jsoup.parse(html);
        
        String title = doc.title();
        System.out.println("Titre de la page: " + title);
        
        Elements paragraphs = doc.select("p");
        for (Element p : paragraphs) {
            System.out.println("Paragraphe: " + p.text());
        }
    }
}
```

Sortie attendue :
```
Titre de la page: Exemple
Paragraphe: Ceci est un paragraphe.
```

## Plongée profonde :
Historiquement, l'analyse du HTML était une affaire complexe, surtout avant des standards comme DOM. On utilisait des expressions régulières (mauvaise idée pour le HTML complexe!) ou des parsers XML (pas toujours compatibles avec les subtilités du HTML). 

Aujourd'hui, on a des outils comme Jsoup qui facilitent grandement la tâche. Mais pourquoi préférer Jsoup à d'autres ? D'abord, il tolère bien le HTML "mal formé". Ensuite, il est rapide et respecte la sémantique des CSS pour sélectionner des éléments.

Pour implémenter le parsing, Jsoup crée son propre DOM en Java. Cela vous permet de manipuler facilement les éléments et cela ressemble beaucoup à manipuler du HTML avec jQuery.

Alternativement, il y a d'autres bibliothèques comme HtmlUnit ou même des API natives en Java comme XPath qui peuvent servir à parser le HTML. Choisir l'une ou l'autre dépend de vos besoins spécifiques.

## Voir aussi :
- [Jsoup Official Documentation](https://jsoup.org/)
- [HTML parsing and scraping in Java with Jsoup – Baeldung](https://www.baeldung.com/java-with-jsoup)
- [What is the best HTML parser for Java? – Stack Overflow](https://stackoverflow.com/questions/3152138/what-is-the-best-html-parser-java-library)
