---
title:                "Extraction de sous-chaînes"
html_title:           "Arduino: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Extraire des sous-chaînes signifie prendre une portion spécifique d'une chaîne existante. Les programmeurs font cela pour manipuler, analyser ou comparer des segments de données textuelles dans leurs codes.

## Comment Faire:
Voici un exemple simple sur comment extraire une sous-chaîne en utilisant la méthode `substring` en Java:

```Java
public class Main {
  public static void main(String[] args) {
    String str = "Bonjour, Monde!";
    String substr = str.substring(9, 14);
    System.out.println(substr);
  }
}
```
Sortie:
```
Monde
```
Dans cet exemple, `str.substring(9, 14)` extrait les caractères de l'indice 9 à l'indice 13 (14-1) dans la chaîne `str`.

## Immersion Profonde:
Historiquement, l'extraction de sous-chaînes en Java a débuté avec `String.substring`. Ces jours-ci, nous avons également `String.subSequence` qui fait fondamentalement la même chose mais retourne un `CharSequence`.

La méthode `substring` fonctionne en prenant les indices de début et de fin comme arguments et en renvoyant la sous-chaîne correspondante. Notez que l'indice de fin est exclusif. Si seul l'indice de début est fourni, la méthode renvoie la sous-chaîne à partir de cet indice jusqu'à la fin de la chaîne.

Alternativement, vous pouvez utiliser `split` pour diviser une chaîne en plusieurs sous-chaînes basées sur un délimiteur, ou `Pattern` et `Matcher` pour une extraction plus complexe et flexible basée sur des expressions régulières.

Conseil: Pour des performances optimales, évitez de créer inutilement des sous-chaînes, car chaque sous-chaîne en Java crée un nouvel objet, ce qui prend de la mémoire.

## Voir Aussi
Consultez les liens ci-dessous pour plus d'informations sur les opérations liées aux chaînes en Java:

1. Documentation Oracle pour [String](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html)
3. Article [Effective Java, Item 54 – Use standard libraries](https://modernpathshala.com/Article/1176/Item-54-Use-standard-libraries) qu'explique pourquoi et comment utiliser efficacement les bibliothèques Java standard.