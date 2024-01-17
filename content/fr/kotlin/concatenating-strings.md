---
title:                "Concaténation de chaînes de caractères"
html_title:           "Kotlin: Concaténation de chaînes de caractères"
simple_title:         "Concaténation de chaînes de caractères"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

# Qu'est-ce que la "concaténation de chaînes" et pourquoi les programmeurs le font-ils?

La concaténation de chaînes est le processus de combiner plusieurs chaînes de caractères en une seule chaîne plus longue. Les programmeurs utilisent généralement cette technique lorsqu'ils ont besoin de créer une chaîne de caractères à partir de plusieurs parties différentes.

# Comment procéder:

Voici un exemple de code Kotlin qui utilise la concaténation de chaînes pour créer une phrase à partir de plusieurs variables:

```Kotlin
val nom = "Pierre"
val age = 35
val profession = "développeur"

val phrase = "Bonjour, je m'appelle $nom, j'ai $age ans et je suis $profession."

println(phrase)
```

**Sortie:**
```
Bonjour, je m'appelle Pierre, j'ai 35 ans et je suis développeur.
```

# Profondeur de plongée:

La concaténation de chaînes est une technique couramment utilisée depuis les débuts de la programmation informatique. Elle reste toujours très pratique pour créer des chaînes de caractères dynamiques. Cependant, avec l'avènement des langages de programmation modernes tels que Kotlin, il existe d'autres alternatives telles que les fonctions de formattage de chaînes, qui peuvent rendre le code plus lisible dans certains cas.

# Voir aussi:

Pour en savoir plus sur la concaténation de chaînes et sur les différentes façons de manipuler des chaînes en Kotlin, consultez la documentation officielle de Kotlin: https://kotlinlang.org/docs/reference/strings.html