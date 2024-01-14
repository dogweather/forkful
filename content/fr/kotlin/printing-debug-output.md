---
title:    "Kotlin: Imprimer la sortie de débogage"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

##Pourquoi

Le débogage est un élément crucial de la programmation, et la sortie de débogage est un outil essentiel dans la boîte à outils d'un développeur. Cela permet de vérifier si le code fonctionne correctement, de détecter et de résoudre les erreurs et les problèmes. Sans la sortie de débogage, il peut être difficile de comprendre ce qui se passe dans le code et de trouver des solutions.

##Comment faire

En Kotlin, il existe plusieurs façons d'afficher une sortie de débogage. La méthode la plus courante est d'utiliser la fonction `println()` pour imprimer une valeur sur la console.

```Kotlin
fun main() {
    var age = 32
    println(age) //imprime la valeur de la variable "age" sur la console
}
```

Vous pouvez également utiliser la fonction `Log.d()` pour afficher une sortie de débogage dans les applications Android.

```Kotlin
fun onClickButton() {
    var name = "John"
    Log.d("Débogage", "Le nom de l'utilisateur est $name") //imprime un message de débogage avec la valeur de la variable "name"
}
```

Vous pouvez également utiliser des expressions et des conditions pour afficher des messages de débogage en fonction de certaines conditions. Par exemple, cela peut être utile pour vérifier si une condition est remplie ou pour afficher différentes valeurs en fonction de différentes variables.

```Kotlin
fun main() {
    var x = 5
    if(x > 10){
        println("x est plus grand que 10")
    } else{
        println("x est plus petit ou égal à 10")
    }
}
```

##Plongée en profondeur

L'affichage de la sortie de débogage peut également être utile pour suivre l'exécution du code et voir les valeurs des différentes variables à chaque étape. Vous pouvez utiliser la fonction `step()` pour cela.

```Kotlin
fun main(){
    var x = 5
    var y = 10
    println("x = $x et y = $y") //imprime les valeurs initiales des variables
    x += 2
    y *= 2
    println("x = $x et y = $y") //imprime les nouvelles valeurs des variables après les modifications
}
```

Vous pouvez également utiliser la fonction `error()` pour afficher des messages de débogage d'erreurs, ce qui peut être utile pour détecter et résoudre les problèmes dans votre code.

```Kotlin
fun divide(num1: Int, num2: Int) {
    if(num2 == 0){
        error("Impossible de diviser par zéro") //affiche un message d'erreur si la division par zéro est tentée
    } else {
        var result = num1 / num2
        println("Le résultat est $result")
    }
}
```

##Voir aussi

- [Guide de débogage en Kotlin](https://kotlinlang.org/docs/tutorials/debugging.html)
- [Documentation officielle de Kotlin sur la fonction `println()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/println.html)
- [Utiliser la fonction `Log.d()` pour afficher des messages de débogage dans les applications Android](https://developer.android.com/studio/debug/log)- [Guide avancé de débogage en Kotlin](https://kotlinlang.org/docs/tutorials/advanced-debugging.html)