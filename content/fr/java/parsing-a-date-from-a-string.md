---
title:                "Analyse d'une date à partir d'une chaîne de caractères."
html_title:           "Java: Analyse d'une date à partir d'une chaîne de caractères."
simple_title:         "Analyse d'une date à partir d'une chaîne de caractères."
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi?
Le "parsing" d'une date à partir d'une chaîne de caractères est le processus de convertir une date écrite sous forme de chaîne de texte en un format de date reconnaissable par l'ordinateur. Les programmeurs le font pour faciliter le traitement des dates dans leurs codes et pour s'assurer que les données saisies par les utilisateurs sont correctement interprétées.

# Comment faire:
```java
/** transformation d'une date en string en un objet date */
SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy");
try{
    Date date = sdf.parse("01/01/2020");
    System.out.println(date);
}catch(ParseException e){
    System.out.println("La date n'est pas valide.");
}
```

Output: Wed Jan 01 00:00:00 GMT 2020

# Plongée en profondeur:
Le parsing de dates à partir de chaînes de caractères a été une fonctionnalité intégrée dans les langages de programmation depuis longtemps. À l'origine, les programmeurs utilisaient des méthodes dédiées aux dates comme "parseDate()" pour effectuer cette tâche, tandis que les programmeurs Java utilisent actuellement les classes "DateFormat" et "SimpleDateFormat". Bien que ces classes offrent une grande flexibilité dans la conversion de divers formats de dates, elles peuvent être délicates à utiliser et sujettes à des erreurs. Il existe également des alternatives telles que les bibliothèques open-source comme "Joda-Time" qui simplifient le processus de parsing de dates.

# Voir aussi:
- Documentation officielle Oracle sur les classes "DateFormat" et "SimpleDateFormat": https://docs.oracle.com/javase/7/docs/api/java/text/DateFormat.html
- Bibliothèque "Joda-Time": https://www.joda.org/joda-time/
- Tutoriel d'introduction sur le parsing de dates avec Java: https://www.baeldung.com/java-string-to-date