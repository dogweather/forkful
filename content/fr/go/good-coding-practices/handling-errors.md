---
title:                "Gestion des erreurs"
aliases: - /fr/go/handling-errors.md
date:                  2024-02-03T17:58:05.329581-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gestion des erreurs"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/handling-errors.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et pourquoi ?

Gérer les erreurs en Go implique de reconnaître et de répondre aux conditions d'erreur dans votre programme. Les programmeurs s'engagent dans la gestion des erreurs pour assurer que leurs applications peuvent se rétablir avec élégance face à des situations inattendues, menant à un logiciel plus robuste et fiable.

## Comment faire :

En Go, la gestion des erreurs est explicitement gérée à l'aide du type `error`. Les fonctions susceptibles d'échouer retournent une erreur comme leur dernière valeur de retour. Vérifier si cette valeur d'erreur est `nil` vous dira si une erreur s'est produite.

```go
package main

import (
    "errors"
    "fmt"
)

func Compute(value int) (int, error) {
    if value > 100 {
        return 0, errors.New("la valeur doit être de 100 ou moins")
    }
    return value * 2, nil
}

func main() {
    result, err := Compute(150)
    if err != nil {
        fmt.Println("Erreur :", err)
    } else {
        fmt.Println("Résultat :", result)
    }
    
    // Gérer une erreur avec élégance
    anotherResult, anotherErr := Compute(50)
    if anotherErr != nil {
        fmt.Println("Erreur :", anotherErr)
    } else {
        fmt.Println("Résultat :", anotherResult)
    }
}
```

Exemple de sortie pour le code ci-dessus :
```
Erreur : la valeur doit être de 100 ou moins
Résultat : 100
```

Dans cet exemple, la fonction `Compute` renvoie soit une valeur calculée soit une erreur. L'appelant gère l'erreur en vérifiant si `err` est différent de `nil`.

## Approfondissement

L'approche de Go pour la gestion des erreurs est délibérément simple et typée, nécessitant des vérifications explicites des erreurs. Ce concept contraste avec la gestion d'erreurs basée sur des exceptions vue dans des langues comme Java et Python, où les erreurs sont propagées dans la pile d'appels à moins d'être capturées par un gestionnaire d'exception. L'équipe de Go argue que la gestion explicite des erreurs résulte en un code plus clair et fiable, car elle force les programmeurs à adresser immédiatement les erreurs là où elles se produisent.

Cependant, certaines critiques mentionnent que ce modèle peut conduire à un code verbeux, en particulier dans des fonctions complexes avec de nombreuses opérations susceptibles d'erreur. En réponse, des versions plus récentes de Go ont introduit des fonctionnalités de gestion des erreurs plus sophistiquées, telles que l'enveloppement d'erreur, facilitant la fourniture de contexte à une erreur sans perdre les informations sur l'erreur originale. La communauté a également vu des propositions pour de nouveaux mécanismes de gestion des erreurs, tels que check/handle, bien que ceux-ci restent en discussion à ma dernière mise à jour.

La philosophie de gestion des erreurs de Go met l'accent sur la compréhension et la planification des erreurs comme partie intégrante du flux normal du programme. Cette approche encourage le développement de logiciels plus résilients et prévisibles, albeit avec une augmentation potentielle du code standard. Des modèles et bibliothèques alternatifs existent pour rationaliser la gestion des erreurs dans des cas particulièrement complexes, mais le type d'erreur intégré de Go reste la base de la gestion des erreurs dans le langage.
