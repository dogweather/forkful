---
date: 2024-01-20 17:42:44.276980-07:00
description: "Comment faire : Parlons code. Pour effacer des caract\xE8res selon un\
  \ motif, on utilise souvent les expressions r\xE9guli\xE8res avec la classe `Pattern`\
  \ et\u2026"
lastmod: '2024-04-05T21:53:59.125691-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Suppression de caract\xE8res correspondant \xE0 un motif"
weight: 5
---

## Comment faire :
Parlons code. Pour effacer des caractères selon un motif, on utilise souvent les expressions régulières avec la classe `Pattern` et `Matcher` ou plus simplement la méthode `replaceAll` des chaînes de caractères.

```java
import java.util.regex.Pattern;
import java.util.regex.Matcher;

public class DeletePatternCharacters {
    public static void main(String[] args) {
        String input = "Bonjour1, c'est le 2e texte3 à nettoyer4!";
        String regex = "\\d"; // On veut supprimer les chiffres

        // Utilisation de replaceAll
        String cleaned = input.replaceAll(regex, "");
        System.out.println(cleaned);  // Bonjour, c'est le e texte à nettoyer!

        // Utilisation de Pattern et Matcher
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(input);
        String alternativeCleaned = matcher.replaceAll("");
        System.out.println(alternativeCleaned);  // Même résultat ici
    }
}
```

## Pour aller plus loin :
Historiquement, les expressions régulières viennent des concepts mathématiques de langage formel et ont été popularisées dans les utilitaires Unix pour manipuler du texte. En Java, `Pattern` et `Matcher` sont des classes robustes pour gérer les expressions régulières depuis Java 1.4.

Comme alternatives, pour des besoins simples, on peut se passer des regex en utilisant des méthodes comme `replace` sur les chaînes. Pour des traitements plus complexes, il y a des bibliothèques comme Apache Commons Lang avec `StringUtils`, qui offrent des fonctions plus avancées.

L'implémentation en Java utilise une machine d'états pour évaluer les expressions régulières, ce qui peut être gourmand en performances si mal utilisé. C'est pourquoi réfléchir au motif et tester son efficacité est important.

## À voir également :
Pour creuser le sujet, voici quelques liens utiles :

- [Documentation Oracle sur les expressions régulières en Java](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
- [Apache Commons Lang](https://commons.apache.org/proper/commons-lang/)
- [StackOverflow: Uses of Java Pattern and Matcher](https://stackoverflow.com/questions/4450045/difference-between-string-replace-and-replaceall)

Et n'oubliez pas, le meilleur moyen de se familiariser avec les regex c'est la pratique ! Donc, à vos IDE et bon codage.
