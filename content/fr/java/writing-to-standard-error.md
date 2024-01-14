---
title:    "Java: Écriture vers l'erreur standard"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

#Pourquoi

L'écriture sur la sortie d'erreur standard (standard error) est un moyen rapide et facile de déboguer votre code Java. Cela vous permet d'afficher des messages d'erreur et des informations de débogage directement dans votre terminal, sans avoir à interrompre l'exécution de votre programme.

#Comment Faire

Voici un exemple simple montrant comment écrire sur la sortie d'erreur standard en utilisant la méthode ```System.err.println()```:

```
public class Main {
    public static void main(String[] args) {
        System.err.println("Ceci est un message d'erreur");
        
        int a = 10;
        int b = 0;
        try {
            int result = a / b;
            System.out.println("Le résultat est : " + result);
        } catch (ArithmeticException e) {
            System.err.println("Une erreur de division par zéro a été détectée");
        }
    }
}
```

Lorsque vous exécutez ce code, vous obtenez l'output suivant:

```
Ceci est un message d'erreur
Une erreur de division par zéro a été détectée
```

Comme vous pouvez le voir, le message d'erreur est imprimé en rouge dans votre terminal grâce à l'utilisation de ```System.err.println()```.

#Plongée En Profondeur

Il est important de noter que la sortie standard (standard output) et la sortie d'erreur standard (standard error) sont deux canaux de communication distincts dans un programme Java. La sortie standard est utilisée pour l'affichage des résultats ou des messages informatifs, tandis que la sortie d'erreur standard est dédiée aux messages d'erreur et de débogage.

Ainsi, en utilisant la méthode ```System.err.println()```, vous pouvez spécifiquement écrire sur la sortie d'erreur standard pour séparer clairement les différents types de messages dans votre programme.

#Voir Aussi

- [Documentation officielle de System](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html)
- [Guide complet du débogage en Java](https://www.baeldung.com/java-debugging)
- [Utiliser les entrées et sorties en Java](https://www.tutorialspoint.com/java/java_files_io.htm)