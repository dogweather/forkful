---
title:                "Utilisation d'un débogueur"
date:                  2024-01-26T03:49:50.074976-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation d'un débogueur"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/using-a-debugger.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Plonger dans un débogueur, c'est tout simplement parcourir votre code pas à pas, observer les rouages en action et attraper ces ennuyeux bugs sur le fait. Les programmeurs utilisent des débogueurs car ce sont les outils de détective qui nous aident à comprendre où les choses tournent mal sans nous arracher les cheveux.

## Comment faire :
Voici un petit avant-goût du débogage en Kotlin avec IntelliJ IDEA - le Sherlock Holmes des IDEs :

```kotlin
fun main() {
    val mysteryNumber = 42
    var guess = 0

    while (guess != mysteryNumber) {
        println("Devinez le nombre : ")
        guess = readLine()?.toIntOrNull() ?: continue // Ignorer les entrées incorrectes

        // Placez un point d'arrêt ici pour observer 'guess' en action
        if (guess < mysteryNumber) {
            println("Trop bas !")
        } else if (guess > mysteryNumber) {
            println("Trop haut !")
        }
    }

    println("Vous l'avez trouvé ! Le nombre mystère était $mysteryNumber")
}
```

Sortie du débogueur :
```
Devinez le nombre : 
10
Trop bas !
Devinez le nombre : 
50
Trop haut !
Devinez le nombre : 
42
Vous l'avez trouvé ! Le nombre mystère était 42
```

## Exploration en profondeur
Les débogueurs sont dans le jeu depuis les années 50. À cette époque, ils étaient assez primitifs, et déboguer pouvait concerner davantage le matériel que le logiciel. De nos jours, un débogueur comme celui d'IntelliJ IDEA nous permet de placer des points d'arrêt, de parcourir le code ligne par ligne, et d'inspecter l'état des variables à notre guise.

Bien que le débogueur d'IntelliJ soit très pratique pour Kotlin, ce n'est pas le seul poisson dans la mer. Il existe une gamme d'alternatives comme Logcat pour le développement Android, ou des outils en ligne de commande comme jdb pour les minimalistes. La magie sous le capot ici concerne principalement l'Interface de l'Outil JVM (JVMTI), qui permet aux débogueurs d'interagir avec la Machine Virtuelle Java, en gardant les développeurs Kotlin dans la boucle.

## Voir aussi
- Documentation du débogueur IntelliJ IDEA : [https://jetbrains.com/idea/](https://www.jetbrains.com/idea/features/debugger.html)
- Documentation Kotlin sur le débogage : [https://kotlinlang.org/docs/debugging.html](https://kotlinlang.org/docs/debugging.html)
- Les racines du débogage dans l'histoire : [http://history-computer.com/Internet/Maturing/Debugging.html](http://history-computer.com/Internet/Maturing/Debugging.html)