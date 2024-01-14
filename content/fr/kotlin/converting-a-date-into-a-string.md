---
title:    "Kotlin: Conversion d'une date en chaîne de caractères"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# Pourquoi: Convertir une date en chaîne de caractères
La conversion d'une date en chaîne de caractères peut être utile dans de nombreuses situations, notamment pour afficher une date dans un format spécifique ou pour enregistrer une date dans une base de données sous forme de texte.

# Comment faire
Pour convertir une date en chaîne de caractères en utilisant Kotlin, il existe plusieurs façons de le faire. Voici deux exemples utilisant les classes `SimpleDateFormat` et `DateTimeFormatter`:

```Kotlin
// Importer les classes nécessaires
import java.text.SimpleDateFormat
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

// Définir le format de la date souhaitée en chaîne de caractères
val format = "dd/MM/yyyy HH:mm:ss"

// Exemple 1: Utiliser la classe SimpleDateFormat
val date = LocalDateTime.now()
val sdf = SimpleDateFormat(format)
val stringDate = sdf.format(date)
println(stringDate) // Affiche: 23/07/2021 15:52:34

// Exemple 2: Utiliser la classe DateTimeFormatter (recommandé à partir de Java 8)
val dateTime = LocalDateTime.now()
val formatter = DateTimeFormatter.ofPattern(format)
val stringDateTime = dateTime.format(formatter)
println(stringDateTime) // Affiche: 23/07/2021 15:52:34
```

La sortie de ces deux exemples sera la même, affichant la date et l'heure actuelles dans le format spécifié.

# Plongée profonde
Il est important de noter que la classe `SimpleDateFormat` est obsolète à partir de Java 8 et qu'il est recommandé d'utiliser la classe `DateTimeFormatter` pour convertir une date en chaîne de caractères. Cette dernière offre de meilleures performances et est plus sûre à utiliser dans des environnements multithreadés.

De plus, il existe de nombreux autres formats de dates et d'heures prédéfinis dans `DateTimeFormatter`, tels que `ISO_DATE` ou `ISO_TIME`, qui peuvent être utiles en fonction de vos besoins.

# Voir aussi
- [Documentation officielle de Kotlin sur la classe SimpleDateFormat](https://kotlinlang.org/api/latest/jvm/stdlib/java.text/-simple-date-format/)
- [Documentation officielle de Kotlin sur la classe DateTimeFormatter](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-date-time-formatter/)
- [Guide de conversion de dates et d'heures avec Kotlin](https://www.baeldung.com/kotlin/date-time-conversion)