---
title:                "Convertir une date en une chaîne de caractères"
html_title:           "Kotlin: Convertir une date en une chaîne de caractères"
simple_title:         "Convertir une date en une chaîne de caractères"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Tu pourrais te demander pourquoi tu voudrais convertir une date en chaîne de caractères en utilisant Kotlin. Eh bien, cela peut être utile pour afficher des dates dans un format spécifique dans tes applications, ou pour manipuler des données de type date dans une base de données.

## Comment faire

Pour convertir une date en chaîne de caractères en utilisant Kotlin, tu peux simplement utiliser la fonction `format` de la classe `SimpleDateFormat`. Voici un exemple de code qui te montrera comment faire:

```
fun convertDateToString(date: Date): String {
    val format = SimpleDateFormat("dd/MM/yyyy") // spécifie le format de la date que tu veux
    return format.format(date) // convertit la date en chaîne de caractères selon le format spécifié
}

val myDate = Date() // crée une nouvelle instance de l'objet Date avec la date actuelle
println(convertDateToString(myDate)) // affiche la date actuelle au format "dd/MM/yyyy"
```

Output:
```
20/01/2022
```

## Plongeons plus profondément

Si tu veux changer le format de la date en fonction de ta localisation ou en utilisant un format spécifique, tu peux utiliser la classe `Locale` pour spécifier la langue et le pays, ainsi que la classe `DateFormatSymbols` pour spécifier les symboles à utiliser pour le format. De plus, tu peux également utiliser la fonction `parse` pour convertir une chaîne de caractères en date. Voici un exemple de code pour te montrer ces fonctionnalités:

```
fun convertDateToString(date: Date, language: String, country: String): String {
    val format = SimpleDateFormat("dd/MM/yyyy", Locale(language, country)) // spécifie le format et la localisation
    return format.format(date) // convertit la date en chaîne de caractères selon le format et la localisation spécifiés
}

fun convertStringToDate(date: String, language: String, country: String): Date {
    val format = SimpleDateFormat("dd/MM/yyyy", Locale(language, country)) // spécifie le format et la localisation
    return format.parse(date) // convertit la chaîne de caractères en date selon le format et la localisation spécifiés
}

val myDate = Date() // crée une nouvelle instance de l'objet Date avec la date actuelle

println(convertDateToString(myDate, "fr", "FR")) // affiche la date actuelle au format spécifique à la France
println(convertStringToDate("20/01/2022", "fr", "FR")) // convertit la chaîne de caractères en date au format spécifique à la France
```

Output:
```
20/01/2022
Thu Jan 20 00:00:00 CET 2022
```

## Voir aussi

- [How to convert Kotlin Date to String](https://www.programiz.com/kotlin-programming/examples/date-string)
- [Kotlin SimpleDateFormat class](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-simple-date-format/index.html)
- [Kotlin Locale class](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-locale/index.html)