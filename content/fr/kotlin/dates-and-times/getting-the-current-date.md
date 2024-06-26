---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:08.147127-07:00
description: "Comment faire : Kotlin n'a pas sa propre API de date et d'heure, mais\
  \ s'appuie sur la biblioth\xE8que standard Java pour cette fonctionnalit\xE9. Voici\
  \ comment\u2026"
lastmod: '2024-03-13T22:44:57.751101-06:00'
model: gpt-4-0125-preview
summary: "Kotlin n'a pas sa propre API de date et d'heure, mais s'appuie sur la biblioth\xE8\
  que standard Java pour cette fonctionnalit\xE9."
title: Obtenir la date actuelle
weight: 29
---

## Comment faire :


### En utilisant Kotlin Standard
Kotlin n'a pas sa propre API de date et d'heure, mais s'appuie sur la bibliothèque standard Java pour cette fonctionnalité. Voici comment vous pouvez obtenir la date actuelle :

```kotlin
import java.time.LocalDate

fun main() {
    val aujourd'hui = LocalDate.now()
    println("Date d'aujourd'hui : $aujourd'hui")
}
```

**Exemple de sortie :**
```
Date d'aujourd'hui : 2023-04-05
```

### En utilisant java.util.Date
Pour les opérations nécessitant à la fois la date et l'heure, vous pourriez préférer `java.util.Date`.

```kotlin
import java.util.Date

fun main() {
    val dateActuelle = Date()
    println("Date et Heure actuelles : $dateActuelle")
}
```

**Exemple de sortie :**
```
Date et Heure actuelles : Mer Apr 05 15:20:45 GMT 2023
```

### En utilisant la bibliothèque Joda-Time
Avant que Java 8 n'introduise une nouvelle API de date et d'heure, Joda-Time était la norme de facto pour les opérations de date-heure en Java et Kotlin. Même si ce n'est plus nécessaire pour de nombreux projets, certains peuvent toujours l'utiliser pour des raisons d'héritage ou par préférence personnelle.

Ajoutez la bibliothèque Joda-Time au fichier build.gradle de votre projet :
```
implementation 'joda-time:joda-time:2.10.10'
```

```kotlin
import org.joda.time.LocalDate

fun main() {
    val aujourd'hui = LocalDate.now()
    println("Date d'aujourd'hui : $aujourd'hui")
}
```

**Exemple de sortie :**
```
Date d'aujourd'hui : 2023-04-05
```

### En utilisant ThreeTenABP pour Android
Pour le développement Android, il est recommandé d'utiliser la rétroportage de l'API Time Java via le projet ThreeTen Android Backport pour les versions antérieures à l'API Level 26 d'Android.

Ajoutez la dépendance au fichier build.gradle de votre application :
```
implementation 'com.jakewharton.threetenabp:threetenabp:1.3.1'
```

Initialisez-le dans votre classe Application :
```kotlin
import android.app.Application
import com.jakewharton.threetenabp.AndroidThreeTen

class MonApp : Application() {
    override fun onCreate() {
        super.onCreate()
        AndroidThreeTen.init(this)
    }
}
```

Ensuite, vous pouvez l'utiliser ainsi :
```kotlin
import org.threeten.bp.LocalDate

fun main() {
    val aujourd'hui = LocalDate.now()
    println("Date d'aujourd'hui : $aujourd'hui")
}
```

**Exemple de sortie :**
```
Date d'aujourd'hui : 2023-04-05
```
