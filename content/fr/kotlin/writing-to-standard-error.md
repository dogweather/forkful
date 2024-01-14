---
title:                "Kotlin: Écriture vers la sortie d'erreur standard"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi 

Si vous êtes un programmeur Kotlin, vous avez probablement déjà entendu parler de l'écriture sur l'erreur standard (standard error en anglais). Mais cela peut sembler un concept quelque peu mystérieux pour les nouveaux arrivants en Kotlin. Alors, pourquoi quelqu'un voudrait-il écrire sur l'erreur standard ? Jetons un coup d'œil.

## Comment faire 

Pour écrire sur l'erreur standard en Kotlin, nous utilisons la fonction `System.err.println()`. Cette fonction permet d'afficher un message sur l'erreur standard plutôt que sur la sortie standard (standard output). Regardons un exemple concret :

```
val name = "Jean"
System.err.println("Bonjour $name, quelle magnifique journée !")
```

Dans cet exemple, nous utilisons la fonction `System.err.println()` pour afficher notre message de salutation sur l'erreur standard. Lorsque nous exécutons ce code, nous obtenons l'output suivant:

```
Bonjour Jean, quelle magnifique journée !
```

Comme vous pouvez le voir, le message est imprimé sur l'erreur standard plutôt que sur la sortie standard.

## Plongeons plus profondément

Maintenant que nous savons comment écrire sur l'erreur standard, nous pouvons nous demander dans quels cas cela pourrait être utile. La principale raison pour laquelle vous voudriez écrire sur l'erreur standard est si vous voulez séparer vos messages de log en deux catégories : les messages d'erreur et les messages d'information. Cela peut être particulièrement utile lorsque vous travaillez sur un projet en équipe et que vous voulez que les développeurs puissent rapidement voir et corriger les erreurs lors de la lecture des messages de log.

De plus, écrire sur l'erreur standard peut également être utile pour déboguer du code. Si vous voulez voir si une partie spécifique de votre programme est exécutée, vous pouvez utiliser la fonction `System.err.println()` pour afficher un message sur l'erreur standard et vérifier si le message est imprimé ou non. Cela peut vous aider à identifier les problèmes et à les résoudre plus rapidement.

## Voir aussi 

Si vous voulez en savoir plus sur l'écriture sur l'erreur standard en Kotlin, voici quelques ressources utiles :

- Documentation officielle de Kotlin sur l'écriture sur l'erreur standard : https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/system.err.html
- Un tutoriel sur l'utilisation de la fonction `System.err.println()` : https://blog.frankel.ch/debugging-kotlin-std/
- Un exemple de l'utilisation de l'erreur standard pour gérer les exceptions en Kotlin : https://www.baeldung.com/kotlin-exceptions

Maintenant que vous savez comment écrire sur l'erreur standard en Kotlin, vous pouvez l'utiliser pour améliorer vos messages de log et déboguer votre code plus efficacement. N'hésitez pas à essayer cette fonctionnalité dans vos prochains projets !