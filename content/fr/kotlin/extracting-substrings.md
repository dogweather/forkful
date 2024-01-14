---
title:    "Kotlin: Extraction de sous-chaînes"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes programmeur en Kotlin, vous avez probablement déjà eu besoin d'extraire une partie spécifique d'une chaîne de caractères. Cela peut sembler banal, mais l'extraction de sous-chaînes est une compétence très utile à maîtriser. Elle vous permettra de manipuler efficacement les données et de gagner du temps dans vos projets de programmation.

## Comment faire

Pour extraire une sous-chaîne en Kotlin, nous utilisons la fonction `substring` avec deux paramètres : l'index de début et l'index de fin de la sous-chaîne que nous voulons extraire. Voici un exemple de code :

```Kotlin
val str = "Bonjour le monde"
val substr = str.substring(8, 11)
println(substr)
```

Dans cet exemple, nous avons extrait la sous-chaîne "le" de la chaîne d'origine, en spécifiant l'index de début 8 (première lettre de "le") et l'index de fin 11 (dernière lettre de "le"). Le résultat de l'affichage sera donc "le".

Il est également possible d'utiliser des index négatifs pour commencer l'extraction à partir de la fin de la chaîne. Par exemple, `str.substring(5, -1)` extraiera les lettres "jour le mond" de la chaîne d'origine.

Il est également possible d'extraire une sous-chaîne à partir d'un index donné jusqu'à la fin de la chaîne, en omettant le deuxième paramètre de la fonction `substring`. Par exemple, `str.substring(3)` extraira tout ce qui suit la troisième lettre de la chaîne d'origine, soit "jour le monde".

## Plongée en profondeur

La fonction `substring` utilise des index de 0 à n pour indiquer les emplacements dans une chaîne de caractères. Le premier caractère correspond à l'index 0, le deuxième à l'index 1, et ainsi de suite. Il est important de garder cela à l'esprit lorsque vous extrayez des sous-chaînes pour vous assurer d'obtenir le résultat souhaité.

De plus, la fonction `substring` ne modifie pas la chaîne d'origine, elle renvoie simplement la sous-chaîne extraite. Si vous souhaitez modifier la chaîne d'origine, vous pouvez utiliser la fonction `replaceRange` avec les mêmes paramètres que `substring`.

## Voir aussi

- [Documentation officielle de la fonction substring en Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html)
- [Article sur la manipulation des chaînes en Kotlin](https://www.baeldung.com/kotlin/working-strings)
- [Tutoriel sur Kotlin pour les débutants](https://www.tutorialspoint.com/kotlin/kotlin_quick_guide.htm)