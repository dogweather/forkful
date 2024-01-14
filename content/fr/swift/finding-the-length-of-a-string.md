---
title:    "Swift: Trouver la longueur d'une chaîne"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur débutant, vous vous demandez peut-être pourquoi il est important de connaître la longueur d'une chaîne de caractères. Eh bien, la réponse est simple : connaître la longueur d'une chaîne de caractères peut vous aider à traiter correctement des données et à manipuler des chaînes de manière efficace dans vos programmes Swift.

## Comment faire

Il existe plusieurs moyens de trouver la longueur d'une chaîne de caractères en Swift. Voici deux méthodes couramment utilisées :

```Swift
let phrase = "Bonjour le monde"
let longueur = phrase.count

print(longueur)
// Output : 16
```

Dans cet exemple, nous utilisons la méthode `count` pour trouver la longueur de notre chaîne de caractères `phrase`. Cette méthode renvoie simplement le nombre de caractères dans la chaîne, ce qui est égal à 16 dans notre cas.

Une autre méthode populaire pour trouver la longueur d'une chaîne de caractères est d'utiliser la propriété `length` de la classe `NSString`. Voici un exemple de code avec cette méthode :

```Swift
let phrase = "Bonjour le monde"
let longueur = NSString(string: phrase).length

print(longueur)
// Output : 16
```

Comme vous pouvez le voir, les deux méthodes donnent le même résultat. Vous pouvez donc choisir celle qui vous convient le mieux en fonction de votre projet et de votre préférence personnelle.

## Approfondissement

Si vous vous intéressez à la façon dont ces méthodes fonctionnent en interne, voici quelques informations supplémentaires. La méthode `count` utilise en fait la propriété `utf16` de la chaîne de caractères pour compter le nombre de caractères. Cette propriété contient un tableau des valeurs UTF-16 pour chaque caractère de la chaîne. La méthode `length` utilise quant à elle la fonction `lengthOfBytes` pour obtenir le nombre de bytes dans la chaîne, puis divise ce nombre par deux pour obtenir le nombre de caractères UTF-16.

Il est important de comprendre les différences entre ces deux méthodes, car dans certains cas, elles peuvent donner des résultats différents. Par exemple, si votre chaîne de caractères contient des caractères Unicode, la méthode `count` risque de ne pas compter correctement le nombre de caractères.

En fin de compte, quelle que soit la méthode que vous utilisez, assurez-vous de bien comprendre comment elle fonctionne pour éviter les erreurs et les problèmes dans votre code.

## Voir aussi

Pour en savoir plus sur la manipulation de chaînes de caractères en Swift, vous pouvez consulter ces liens :

- Guide officiel de Swift sur les chaînes de caractères : https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html
- Tutoriel sur la manipulation de chaînes en Swift : https://www.raywenderlich.com/76341/swift-strings-modern-swift-tutorial
- Comparaison entre les méthodes `count` et `length` : https://www.hackingwithswift.com/articles/162/whats-the-difference-between-string-count-and-nsstring-length

J'espère que cet article vous a été utile dans votre apprentissage de Swift ! N'hésitez pas à explorer davantage et à expérimenter avec ces méthodes pour mieux les comprendre. Bonne programmation !