---
title:    "Kotlin: Trouver la longueur d'une chaîne de caractères"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# Pourquoi

Trouver la longueur d'une chaîne de caractères peut sembler une tâche simple, mais cela peut être extrêmement utile en programmation. Que ce soit pour vérifier la validité d'un mot de passe ou pour compter le nombre de caractères dans un texte, la connaissance de la longueur d'une chaîne peut faciliter de nombreuses tâches dans votre code.

# Comment faire

Pour trouver la longueur d'une chaîne en Kotlin, vous pouvez utiliser la fonction prédéfinie "length". Voici un exemple de code qui montre comment l'utiliser :

```Kotlin
val mot = "Bonjour"
println("La longueur de '$mot' est ${mot.length}")
```

La sortie de ce code serait "La longueur de 'Bonjour' est 7". Vous pouvez également utiliser la méthode "length()" sur un objet String pour obtenir la longueur de cette chaîne spécifique.

Il est également possible d'utiliser la notation d'index pour accéder à des éléments spécifiques dans une chaîne. Par exemple, en utilisant "string[position]", vous pouvez obtenir le caractère à la position donnée dans la chaîne. Vous pouvez également utiliser la méthode "lastIndex" pour obtenir l'index du dernier caractère dans une chaîne. Voici un exemple de code pour mieux comprendre :

```Kotlin
val chanson = "Imagine"
val premierCaractere = chanson[0]
val dernierCaractere = chanson[chanson.lastIndex]
println("Le premier caractère de '$chanson' est '$premierCaractere' et le dernier est '$dernierCaractere'")
```

La sortie de ce code serait "Le premier caractère de 'Imagine' est 'I' et le dernier est 'e'".

# Plongée en profondeur

En coulisses, la fonction "length" utilise la propriété "length" de l'objet String, qui renvoie une valeur entière représentant le nombre de caractères dans la chaîne. Pour les chaînes en Unicode, cela peut différer de la longueur visuelle de la chaîne, car certains caractères peuvent être représentés par plusieurs codes.

Il est également important de noter que la fonction "length" ne compte pas les caractères null ou de contrôle, elle ne mesure que les caractères visibles dans la chaîne.

# Voir aussi

* [Documentation officielle sur la fonction length en Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/length.html)
* [Article sur les strings en Kotlin](https://blog.kotlin-academy.com/strings-in-kotlin-94adba2e95c2)
* [Chaînes en Unicode vs en ASCII](https://www.cs.toronto.edu/~hehner/utf/)