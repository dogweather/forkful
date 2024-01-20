---
title:                "Analyse syntaxique de HTML"
html_title:           "Bash: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/parsing-html.md"
---

{{< edit_this_page >}}

## Qu'est-ce & Pourquoi?

L'analyse de HTML est le processus de conversion d'un document HTML en un arbre d'éléments structuré. Les programmeurs le font pour manipuler, extraire ou comprendre les données contenues dans le HTML.

## Comment faire:

Examinons un exemple simple où nous utilisons la bibliothèque Jsoup pour analyser un document HTML:

```Java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

public class Main {
  public static void main(String[] args) {
    String html = "<html><body><p>Bonjour tout le monde</p></body></html>";
    Document doc = Jsoup.parse(html);
    System.out.println(doc.text());
  }
}
```

Si vous exécutez le code ci-dessus, la sortie sera:

```Java
Bonjour tout le monde
```

Maintenant, si nous voulons extraire tous les paragraphe d'un document HTML:

```Java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

public class Main {
  public static void main(String[] args) {
    String html = "<html><body><p>Bonjour tout le monde</p><p>Soyez les bienvenus</p></body></html>";
    Document doc = Jsoup.parse(html);
    Elements paragraphs = doc.select("p");
    for (Element paragraph : paragraphs) {
      System.out.println(paragraph.text());
    }
  }
}
```

La sortie sera :

```Java
Bonjour tout le monde
Soyez les bienvenus
```

## Plongée profonde

L'analyse de HTML est nécessaire depuis l'adoption massive du Web dans les années 90. Il y avait et il y a toujours une nécessité d'interagir avec le HTML d'une manière plus structurée. 
Une alternative à Jsoup pourrait être le `DOM Parsing API` intégré dans `Java`, mais il est généralement considéré comme plus verbeux et plus difficile à utiliser.

Au niveau d'implémentation, Jsoup utilise une analyse syntaxique ascendante, dans laquelle il lit le HTML de gauche à droite et crée un arbre représentatif, permettant à l'utilisateur d'interagir avec le HTML de manière structurée.

## Pour aller plus loin

- [Documentation officielle de Jsoup](https://jsoup.org/)
- [API Java DOM Parsing](https://docs.oracle.com/javase/tutorial/jaxp/dom/index.html)

N'hésitez pas à consulter ces ressources pour une compréhension plus approfondie de l'analyse de HTML en Java.