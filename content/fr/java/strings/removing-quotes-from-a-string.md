---
date: 2024-01-26 03:39:40.437962-07:00
description: "Comment faire : Retirons ces guillemets ennuyeux de notre texte. Nous\
  \ utiliserons la m\xE9thode `replace()` pour les corrections rapides et les expressions\u2026"
lastmod: '2024-03-13T22:44:57.623513-06:00'
model: gpt-4-0125-preview
summary: Retirons ces guillemets ennuyeux de notre texte.
title: "Retirer les guillemets d'une cha\xEEne"
weight: 9
---

## Comment faire :
Retirons ces guillemets ennuyeux de notre texte. Nous utiliserons la méthode `replace()` pour les corrections rapides et les expressions régulières (regex) pour les cas plus coriaces.

```java
public class SupprimeurDeGuillemets {
    public static void main(String[] args) {
        String chaineAvecGuillemets = "\"Bonjour, 'Monde'!\"";
        String sansGuillemets = chaineAvecGuillemets.replace("\"", "").replace("'", "");
        System.out.println(sansGuillemets); // Bonjour, Monde !

        // Maintenant avec regex pour les amateurs de motifs
        String chaineAvecGuillemetsMixtes = "\"Java\" et 'Programmation'";
        String chainePropre = chaineAvecGuillemetsMixtes.replaceAll("[\"']", "");
        System.out.println(chainePropre); // Java et Programmation
    }
}
```

## Plongée Profonde
Autrefois, les guillemets dans les chaînes n'étaient pas trop gênants—les systèmes étaient plus simples, et les données moins désordonnées. Avec l'avènement de formats de données complexes (JSON, XML) et la nécessité d'échange de données, la gestion des guillemets est devenue clé. En parlant d'alternatives, bien sûr, vous pourriez écrire un analyseur syntaxique, parcourir chaque caractère et construire une nouvelle chaîne (cela pourrait être amusant un jour de pluie). Il y a aussi des bibliothèques tierces qui peuvent gérer cela avec plus de sophistication, offrant des options pour échapper les caractères au lieu de les supprimer, ou pour traiter différents types de guillemets selon la locale. En termes d'implémentation, gardez à l'esprit que supprimer les guillemets sans contexte peut changer la signification ou la structure des données—toujours considérer le "pourquoi" avant le "comment".

## Voir Aussi
- Pour une plongée plus profonde dans les regex, consultez les documents Java officiels : https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html
- Besoin d'échapper les guillemets au lieu de les supprimer ? Stack Overflow est là pour vous : https://stackoverflow.com/questions/383551/escape-string-for-sql-insert
- Traitement de JSON en Java ? Vous rencontrerez probablement souvent des guillemets. Voici un point de départ : https://www.oracle.com/technical-resources/articles/java/json.html
