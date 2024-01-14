---
title:    "Swift: Comparer deux dates"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Pourquoi 

La comparaison de deux dates est une tâche courante en programmation. Elle peut être utile pour vérifier si deux événements ont eu lieu à des moments différents ou pour trier des données selon leur date. Dans cet article, nous allons explorer comment comparer facilement deux dates en utilisant le langage de programmation Swift.

## Comment faire 

Pour comparer deux dates en Swift, vous pouvez utiliser la méthode `compare()` de la classe `Date`. Elle renvoie une instance de l'énumération `ComparisonResult` qui peut être égal à `.orderedAscending`, `.orderedDescending` ou `.orderedSame` en fonction de la relation entre les deux dates.

Voici un exemple de code pour comparer deux dates et afficher le résultat dans la console :

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"
guard let date1 = dateFormatter.date(from: "25/08/2020"),
      let date2 = dateFormatter.date(from: "30/08/2020") 
else {
    fatalError("Erreur lors de la conversion des dates")
}

let comparison = date1.compare(date2)

switch comparison {
case .orderedAscending:
    print("La première date est antérieure à la deuxième date")
case .orderedDescending:
    print("La première date est postérieure à la deuxième date")
case .orderedSame:
    print("Les deux dates sont identiques")
}
```

Lorsque vous exécutez ce code, vous devriez obtenir la sortie suivante : 

```
La première date est antérieure à la deuxième date
```

Vous pouvez également utiliser des opérateurs de comparaison tels que `< >` et `==` pour comparer deux dates. Activez simplement l'extension `Comparable` pour la classe `Date` en ajoutant `Comparable` après `Date` dans la déclaration de la classe. Ensuite, vous pourrez utiliser les opérateurs de comparaison comme suit :

```Swift
let date1 = Date()
let date2 = Date(timeIntervalSinceNow: 3600)
if date1 < date2 {
    print("La première date est antérieure à la deuxième date")
}
```

Maintenant, vous savez comment comparer des dates en utilisant Swift !

## Plongée en profondeur 

Lorsque vous comparez deux dates en utilisant la méthode `compare()` ou les opérateurs de comparaison, il est important de prendre en compte le fuseau horaire et le calendrier. Si vous ne spécifiez pas un fuseau horaire et un calendrier spécifiques, Swift utilise par défaut le calendrier et le fuseau horaire du système.

De plus, lorsque vous utilisez `DateFormatter` pour convertir une chaîne de caractères en date, assurez-vous de spécifier le même format de date que celui utilisé pour créer la chaîne de caractères. Dans l'exemple précédent, nous avons utilisé le format "dd/MM/yyyy" pour créer la date. Si nous avions utilisé un format différent, la conversion aurait échoué.

## Voir aussi 

- [Documentation officielle de Swift sur la classe Date](https://developer.apple.com/documentation/foundation/date)
- [Tutoriel sur la manipulation des dates en Swift](https://www.appcoda.com/swift-date-manipulation/)