---
title:    "Swift: Écrire vers l'erreur standard"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur iOS, vous connaissez probablement déjà le langage de programmation Swift. Il s'agit du langage principal utilisé pour créer des applications pour les appareils Apple tels que l'iPhone, l'iPad et le Mac. Et si vous avez déjà utilisé Swift, vous avez peut-être remarqué qu'il existe une fonctionnalité appelée "écriture vers l'erreur standard". Dans cet article, nous allons explorer pourquoi vous pourriez vouloir utiliser cette fonctionnalité et comment l'utiliser efficacement dans vos projets Swift.

## Comment faire

L'écriture vers l'erreur standard est une fonctionnalité intégrée de Swift qui vous permet d'afficher du texte dans la console pour aider au débogage de votre code. Pour l'utiliser, vous devez inclure le code suivant dans votre programme :

```Swift
print("Message d'erreur")
```

Ce code affichera le message "Message d'erreur" dans la console. Cela peut sembler simple, mais c'est une fonctionnalité très utile pour détecter et résoudre les erreurs dans votre code.

Par exemple, si vous essayez d'afficher une variable qui n'a pas encore été définie, votre application va planter et vous ne saurez pas ce qui s'est passé. Mais en utilisant l'écriture vers l'erreur standard, vous pouvez afficher la valeur de cette variable dans la console et savoir exactement où se situe le problème.

Vous pouvez également utiliser cette fonctionnalité pour afficher des informations de débogage lors de l'exécution de votre code. Par exemple, vous pouvez afficher les valeurs de vos variables à différents endroits dans votre code pour vous assurer qu'elles correspondent à ce que vous attendez.

## Plongée en profondeur

Maintenant que vous savez comment utiliser l'écriture vers l'erreur standard, il est important de comprendre comment cela fonctionne en coulisses. Lorsque vous utilisez la fonction `print()` pour écrire vers l'erreur standard, vous utilisez en réalité une instance de la classe `TextOutputStream`.

Cette classe gère l'envoi du texte vers l'erreur standard et peut être personnalisée pour modifier le comportement par défaut. Par exemple, vous pouvez créer votre propre classe `TextOutputStream` pour formater vos messages d'erreur de manière plus élégante ou pour les envoyer vers un autre emplacement que la console.

Il est également important de noter que l'écriture vers l'erreur standard n'est pas la même chose que l'écriture vers la sortie standard, qui est gérée par la classe `OutputStream` et peut être utilisée pour écrire dans des fichiers ou des flux réseau.

## Voir aussi

Pour en apprendre davantage sur l'écriture vers l'erreur standard et les autres fonctionnalités de débogage en Swift, consultez les liens suivants :

- [Documentation officielle de Apple sur l'écriture vers l'erreur standard](https://developer.apple.com/documentation/swift/2885064-print)
- [Tutoriel sur l'utilisation de l'écriture vers l'erreur standard en Swift](https://www.hackingwithswift.com/articles/141/8-useful-tips-for-printing-to-the-xcode-console)
- [Liste de toutes les classes de sortie en Swift](https://developer.apple.com/documentation/swift/outputstream)

Merci d'avoir lu cet article sur l'écriture vers l'erreur standard en Swift. Nous espérons que cela vous a aidé à mieux comprendre cette fonctionnalité et à l'utiliser efficacement dans vos projets !