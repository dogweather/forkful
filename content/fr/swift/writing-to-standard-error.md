---
title:                "Swift: Écrire sur la sortie d'erreur standard"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Ecrire sur la sortie d'erreur standard peut sembler être une tâche inutile ou même ennuyeuse, mais c'est en fait une compétence très importante pour tout programmeur Swift. Savoir comment écrire sur la sortie d'erreur standard peut vous aider à déboguer facilement vos programmes et à diagnostiquer les problèmes plus rapidement.

## Comment faire

Voici un exemple simple de code Swift pour écrire sur la sortie d'erreur standard :

```Swift
let errorMessage = "Une erreur s'est produite"
print(errorMessage, to: &stderr)
```

Lorsque vous exécutez ce code, vous verrez l'erreur s'afficher dans la console avec un préfixe "error:". Cela rendra plus facile pour vous de repérer les erreurs et de les traiter rapidement.

N'oubliez pas que vous pouvez également écrire sur la sortie d'erreur standard en utilisant la méthode ```write(to: "")```, mais utiliser la fonction ```print(to: &stderr)``` est généralement plus pratique et plus simple.

## Plongée en profondeur

Ecrire sur la sortie d'erreur standard peut sembler simple, mais il y a en fait quelques nuances à connaître. Par exemple, vous pouvez contrôler ce qui est imprimé avant et après votre message d'erreur en utilisant les paramètres ```terminator``` et ```separator```. Vous pouvez également utiliser la balise ```separator``` pour définir un caractère de séparation personnalisé entre chaque élément imprimé.

Vous pouvez également utiliser la méthode ```FileHandle.standardError``` pour définir un autre flux de sortie d'erreur, au cas où vous voudriez imprimer sur une autre console ou un autre emplacement.

Enfin, il est important de noter que le flux de sortie d'erreur standard n'est pas seulement utile pour la débogage. Vous pouvez également l'utiliser pour imprimer des messages d'erreur ou des avertissements à vos utilisateurs, afin qu'ils sachent ce qui se passe en arrière-plan.

## Voir aussi

- [Documentation officielle de Swift sur la sortie d'erreur standard](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#error-handling)

- [Tutoriel vidéo sur l'utilisation de la sortie d'erreur standard en Swift](https://www.youtube.com/watch?v=TgJYI3xquJU)

- [Un article sur les meilleures pratiques pour la gestion des erreurs en Swift](https://medium.com/ios-os-x-development/error-handling-in-swift-with-do-try-catch-bc7f332ca2cb)