---
title:    "Swift: Écrire sur la sortie standard"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# Pourquoi écrire vers l'erreur standard en Swift

L'écriture vers l'erreur standard, également connu sous le nom d'affichage vers la console, est une technique couramment utilisée par les programmeurs Swift pour déboguer leur code. Elle consiste à afficher des messages dans la console qui peuvent aider à comprendre pourquoi un programme ne fonctionne pas correctement.

# Comment faire

Pour écrire vers l'erreur standard en Swift, vous pouvez utiliser la fonction `print()` avec le paramètre `to` pour spécifier l'erreur standard. Voici un exemple de code :

```Swift
let message = "Hello world!"
print(message, to: &stderr)
```

Lorsque vous exécutez ce code, vous devriez voir le message "Hello world!" affiché dans la console. 

# Plongée en profondeur

Il est également possible d'écrire vers l'erreur standard en utilisant les fonctions `fputs()` et `fflush()`. Voici un exemple de code qui utilise ces fonctions :

```Swift
let message = "Hello world!"
fputs(message, stderr)
fflush(stderr)
```

La fonction `fputs()` envoie le message vers l'erreur standard tandis que `fflush()` assure que le message est affiché immédiatement dans la console.

# Voir aussi

- [Documentation Apple sur l'écriture vers l'erreur standard en Swift](https://developer.apple.com/documentation/swift/standardoutputerrortextinput/error-handling)
- [Tutoriel sur l'écriture vers l'erreur standard en Swift](https://medium.com/@mthilkennedy/swift-printing-to-standard-error-cf5be4195beb)