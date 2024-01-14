---
title:                "Go: Imprimer les sorties de débogage"
programming_language: "Go"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/printing-debug-output.md"
---

{{< edit_this_page >}}

# Pourquoi

Imprimer les informations de débogage est une pratique courante dans la programmation pour aider à identifier les erreurs et les bogues dans le code. Cela peut également être utile pour comprendre le comportement du programme et analyser les performances.

# Comment faire

Pour imprimer des informations de débogage en Go, utilisez la fonction `fmt.Println ()` en passant les variables ou les valeurs que vous souhaitez afficher en tant qu'arguments. Par exemple:
```
fmt.Println("Hello world!")
```
Cela imprimera la phrase "Hello world!" dans la console. Vous pouvez également utiliser `fmt.Printf()` pour imprimer des informations de débogage au format spécifié à l'aide de verbes de formatage. Par exemple:
```
fmt.Printf("Nombre de tentatives: %d", 10)
```
Cela imprimera "Nombre de tentatives: 10" dans la console.

# Plongée en profondeur

Il est important de noter que l'impression de débogage devrait être utilisée à des fins de développement uniquement et ne devrait pas être incluse dans le code final. Cela peut ralentir les performances et rendre votre code difficile à maintenir. 

Une autre astuce utile est d'utiliser `log.Println ()` au lieu de `fmt.Println ()` car cela ajoutera également l'heure et la date à l'impression, ce qui peut être utile pour déboguer des problèmes de synchronisation.

Il est également possible d'imprimer des informations de débogage dans un fichier plutôt que dans la console en utilisant les packages `os` et `io`. Cela peut être utile pour enregistrer un historique de débogage pour une future analyse.

# Voir aussi

- [Documentation Go sur le paquet fmt](https://golang.org/pkg/fmt/)
- [Guide de débogage en Go](https://opensource.com/article/20/4/golang-debugging)
- [Exemples pratiques d'impression de débogage en Go](https://yourbasic.org/golang/print-to-console-log-file/)