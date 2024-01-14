---
title:    "Kotlin: Écrire sur la sortie d'erreur standard"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Ecrire des messages sur la sortie standard d'erreur (standard error) peut sembler fastidieux pour certains programmeurs, mais c'est en réalité une pratique importante et utile. Lisez la suite pour découvrir pourquoi il est important d'engager votre code à écrire sur la sortie standard d'erreur.

## Comment faire

L'écriture sur la sortie standard d'erreur peut sembler complexe, mais avec Kotlin c'est en fait assez simple. Voici un exemple de code pour écrire simplement un message sur la sortie standard d'erreur :

```Kotlin
fun main() {
    System.err.println("Erreur : division par zéro !")
}
```

La sortie de ce code sera : `Erreur : division par zéro !`

Il est également possible d'écrire des variables ou des expressions sur la sortie standard d'erreur. Exemple :

```Kotlin
fun main() {
    val nom = "Jean"
    System.err.println("Bonjour $nom !")
}
```

La sortie de ce code sera : `Bonjour Jean !`

## Plongée en profondeur

Ecrire sur la sortie standard d'erreur est une pratique courante en programmation, car cela permet de signaler des erreurs ou des avertissements lors de l'exécution du code. Cela peut être utile pour les programmeurs lors du débogage du code, ainsi que pour les utilisateurs qui peuvent trouver des informations importantes sur les erreurs rencontrées.

Il est également possible de rediriger la sortie standard d'erreur vers un fichier pour enregistrer les messages importants ou les erreurs rencontrées lors de l'exécution du code. Cela peut être fait en utilisant la classe `PrintStream` et la méthode `setErr()` pour définir le fichier de sortie. Exemple :

```Kotlin
fun main() {
    val fichier = File("erreurs.txt")
    System.setErr(PrintStream(fichier))
    System.err.println("Erreur : division par zéro !")
}
```

La sortie de ce code sera écrite dans le fichier `erreurs.txt` plutôt que sur la sortie standard d'erreur.

## Voir aussi

- [Guide officiel de Kotlin](https://kotlinlang.org/docs/tutorials/command-line.html#standard-inputoutput)
- [Tutoriel sur la redirection de la sortie standard d'erreur en Kotlin](https://www.baeldung.com/kotlin/redirect-system-out-err)
- [Documentation de la classe `PrintStream`](https://docs.oracle.com/javase/7/docs/api/java/io/PrintStream.html) (en anglais)