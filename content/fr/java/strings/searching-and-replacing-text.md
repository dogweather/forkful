---
date: 2024-01-20 17:58:18.107817-07:00
description: "Chercher et remplacer du texte, c'est trouver des bouts de cha\xEEnes\
  \ et les transformer. Les programmeurs le font pour corriger des donn\xE9es, formater\
  \ ou\u2026"
lastmod: '2024-03-13T22:44:57.619440-06:00'
model: gpt-4-1106-preview
summary: "Chercher et remplacer du texte, c'est trouver des bouts de cha\xEEnes et\
  \ les transformer."
title: Recherche et remplacement de texte
weight: 10
---

## How to: "Comment faire :"
```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SearchAndReplace {
    public static void main(String[] args) {
        String originalText = "Les félins sont gracieux et les félins sont agiles.";
        String newText = originalText.replaceAll("félins", "chats");
        System.out.println(newText);
        // Utilisation de regex pour remplacer uniquement les mots complets
        Pattern pattern = Pattern.compile("\\bfélins\\b");
        Matcher matcher = pattern.matcher(originalText);
        String regexReplacedText = matcher.replaceAll("chats");
        System.out.println(regexReplacedText);
    }
}
```
Sortie:
```
Les chats sont gracieux et les chats sont agiles.
Les chats sont gracieux et les chats sont agiles.
```

## Deep Dive "Plongée en profondeur"
Historiquement, la recherche et le remplacement de texte viennent du besoin d'éditer des documents numérisés. Dans les premiers éditeurs de texte, des commandes étaient utilisées pour ces opérations. Aujourd'hui, les langages modernes, tels que Java, permettent ces manipulations avec des méthodes intégrées (`replace`, `replaceAll`, etc.) ou des outils puissants comme les expressions régulières (regex) qui offrent une flexibilité incroyable.

En plus de `replaceAll` qui utilise des regex, Java offre `replace` qui remplace simplement les séquences de caractères littérales. Alors que `replaceAll` est puissant, son utilisation incorrecte peut mener à des erreurs subtiles, surtout avec des regex complexes. Il est crucial de tester les expressions régulières pour s'assurer qu'elles correspondent exactement à ce que l'on souhaite.

D'autres langages offrent des fonctions similaires. Par exemple, en Python, il y a `re.sub()`, et en JavaScript `.replace()` fonctionne avec des chaînes et des regex. Choisir entre ces méthodes dépend de la tâche spécifique, performance et lisibilité du code.

## See Also "Voir aussi"
- [Oracle Java Documentation on Patterns](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
- [Regex101: Outil en ligne pour tester les expressions régulières](https://regex101.com/)
- [Java String API Docs](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
