---
title:    "Swift: Convertissement d'une chaîne en minuscules"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Pourquoi

Lorsque vous écrivez du code en Swift, vous aurez souvent besoin de manipuler des chaînes de caractères. Parfois, vous devrez peut-être convertir ces chaînes en minuscules à des fins de comparaison ou pour une meilleure lisibilité. Dans cet article, nous allons vous montrer comment effectuer cette conversion en utilisant Swift.

# Comment faire

```Swift
let phrase = "Je suis une chaîne de caractères"
let lowerCasePhrase = phrase.lowercased()
print(lowerCasePhrase)
```

L'exemple ci-dessus montre comment vous pouvez convertir une chaîne de caractères en minuscules en utilisant la méthode `lowercased()`. Cela fonctionne en décomposant chaque caractère de la chaîne et en le remplaçant par sa version en minuscules. L'avantage de cette méthode est qu'elle fonctionne avec n'importe quelle langue et prend en compte les caractères accentués.

# Plongée en profondeur

Cette méthode de conversion en minuscules utilise en fait la propriété `Locale` de Swift. Une `Locale` est un ensemble de conventions culturelles et linguistiques qui permettent de formater des données en fonction de la langue et du pays. Ainsi, lorsque vous utilisez `lowercased()`, Swift utilise la locale par défaut de l'appareil pour déterminer la façon dont les caractères doivent être convertis.

Cependant, il est possible de spécifier une locale spécifique à utiliser pour la conversion en minuscules. Par exemple, si vous vouliez convertir une chaîne en minuscules en respectant les règles de l'espagnol, vous pourriez le faire comme ceci :

```Swift
let phrase = "Soy una cadena de caracteres"
let lowerCasePhrase = phrase.lowercased(with: Locale(identifier: "es_ES"))
print(lowerCasePhrase)
// Output: soy una cadena de caracteres
```

Comme vous pouvez le voir, la méthode `lowercased()` accepte un paramètre `with` qui vous permet de spécifier la locale à utiliser. Dans cet exemple, nous utilisons `es_ES` pour l'espagnol de l'Espagne, mais vous pouvez également utiliser `es_MX` pour l'espagnol du Mexique ou tout autre identifiant de locale pris en charge par Swift.

# Voir aussi

- [Documentation officielle de la méthode lowercased()](https://developer.apple.com/documentation/swift/string/extended_grapheme_cluster_operations/3456705-lowercased)
- [Liste des identifiants de locale pris en charge par Swift](https://developer.apple.com/library/archive/documentation/MacOSX/Conceptual/BPInternational/LanguageandLocaleIDs/LanguageandLocaleIDs.html)