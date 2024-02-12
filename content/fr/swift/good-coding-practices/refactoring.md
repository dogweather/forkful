---
title:                "Remaniement de code"
aliases:
- /fr/swift/refactoring/
date:                  2024-01-26T03:36:51.311926-07:00
model:                 gpt-4-0125-preview
simple_title:         "Remaniement de code"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/refactoring.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
La refactorisation est le processus de restructuration du code informatique existant sans en changer le comportement externe. Les programmeurs le font pour nettoyer la base de code, améliorer la lisibilité, la maintenabilité et préparer le terrain pour les futures fonctionnalités avec une dette technique minimale.

## Comment faire :
Commençons par un exemple simple en Swift où nous avons du code répétitif :

```Swift
func printUserDetails(firstName: String, lastName: String, age: Int) {
    print("Prénom : \(firstName)")
    print("Nom : \(lastName)")
    print("Âge : \(age)")
}

func printUserJob(title: String, company: String) {
    print("Intitulé du Poste : \(title)")
    print("Entreprise : \(company)")
}
```

Pour refactoriser cela, cela inclurait la création d'une structure `User` pour encapsuler les attributs de l'utilisateur et ajouter une méthode pour imprimer les détails :

```Swift
struct User {
    let firstName: String
    let lastName: String
    let age: Int
    let jobTitle: String
    let company: String

    func printDetails() {
        print("Prénom : \(firstName)")
        print("Nom : \(lastName)")
        print("Âge : \(age)")
        print("Intitulé du Poste : \(jobTitle)")
        print("Entreprise : \(company)")
    }
}

let user = User(firstName: "John", lastName: "Doe", age: 30, jobTitle: "Développeur logiciel", company: "Solutions Tech")
user.printDetails()
```

### Exemple de sortie :
```
Prénom : John
Nom : Doe
Âge : 30
Intitulé du Poste : Développeur logiciel
Entreprise : Solutions Tech
```

## Plongée profonde
La refactorisation a des racines qui remontent aux premiers jours du génie logiciel, mais le terme a été popularisé à la fin des années 1990, en particulier à travers le livre séminal de Martin Fowler "Refactoring: Improving the Design of Existing Code". Le livre a établi le principe selon lequel le code devrait être continuellement nettoyé par petites étapes plutôt que d'attendre une phase séparée.

Les alternatives au refactoring manuel incluent des outils automatisés et des IDE (Environnements de Développement Intégrés) qui peuvent aider à détecter le code dupliqué, suggérer des simplifications et générer automatiquement des portions de code. Xcode, pour le développement Swift, propose divers outils de refactorisation, tels que renommer et extraire la fonctionnalité de méthode, qui peuvent réduire le potentiel d'erreur humaine dans le processus.

Lors de la mise en œuvre de la refactorisation, il est important d'avoir une suite de tests solide en place. Les tests agissent comme un filet de sécurité, garantissant que les modifications que vous apportez n'introduisent pas de bogues. Cela est vital puisque l'objectif principal de la refactorisation est de modifier la structure interne sans affecter le comportement externe.

## Voir aussi
- ["Refactoring: Improving the Design of Existing Code" par Martin Fowler](http://martinfowler.com/books/refactoring.html)
- [Documentation Swift par Apple](https://swift.org/documentation/)
- [Utilisation des outils de refactorisation Xcode](https://help.apple.com/xcode/mac/current/#/dev91fe7130a)
- [Guide de style Swift de Ray Wenderlich](https://github.com/raywenderlich/swift-style-guide)
