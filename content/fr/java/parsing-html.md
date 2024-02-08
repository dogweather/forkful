---
title:                "Analyse Syntaxique du HTML"
date:                  2024-02-03T19:12:19.183377-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analyse Syntaxique du HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

L'analyse HTML implique de fouiller dans le balisage pour extraire des données comme du texte, des liens ou d'autres éléments. Nous le faisons pour interagir avec ou récupérer le contenu web, automatiser des tâches de navigation, ou tester des applications web.

## Comment faire :

Utilisons Jsoup, une bibliothèque pratique pour travailler avec l'HTML du monde réel. Tout d'abord, ajoutez la dépendance :

```xml
<dependency>
    <groupId>org.jsoup</groupId>
    <artifactId>jsoup</artifactId>
    <version>1.15.2</version>
</dependency>
```

Maintenant, passons à la partie amusante. Voici comment récupérer le titre d'une page web et l'afficher :

```java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

public class HtmlParser {
    public static void main(String[] args) throws IOException {
        String url = "http://example.com";
        Document doc = Jsoup.connect(url).get();
        String title = doc.title();
        System.out.println("Titre : " + title);
    }
}
```

Sortie :

```
Titre : Domaine Exemple
```

Que diriez-vous d'extraire tous les liens ?

```java
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

// ... à l'intérieur du main ou d'une autre méthode
Elements liens = doc.select("a[href]");
for (Element lien : liens) {
    System.out.println("Lien : " + lien.attr("href"));
}
```

## Plongée Profonde

Il fut un temps, l'HTML était dompté par des motifs de regex, une méthode à la fois sujette à erreurs et cauchemardesque pour les documents complexes. Puis est venu Jsoup à la fin des années 2000, offrant une interface semblable à jQuery pour Java pour analyser, traverser et manipuler l'HTML.

Jsoup n'est pas le seul choix. Il y a HtmlUnit pour les tests complets d'applications web avec support JavaScript, mais c'est plus lourd et plus complexe. Pour les tâches légères, Apache Commons Validator est excellent juste pour extraire les URL.

Sous le capot, Jsoup utilise un parseur DOM, qui modélise tout le document en mémoire comme un arbre. Cette approche rend la sélection et la navigation de la structure HTML un jeu d'enfant. De plus, il est indulgent avec l'HTML bâclé, corrigeant les problèmes au fur et à mesure pour garantir une analyse robuste.

Rappelez-vous, lors du scraping, vérifiez toujours le fichier `robots.txt` du site et les conditions d'utilisation pour éviter les ennuis légaux ou le bannissement par IP.

## Voir Aussi

- Documentation officielle de Jsoup : https://jsoup.org/
- HtmlUnit : http://htmlunit.sourceforge.net/
- Apache Commons Validator : https://commons.apache.org/proper/commons-validator/
