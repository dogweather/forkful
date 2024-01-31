---
title:                "Utilisation des expressions régulières"
date:                  2024-01-19
simple_title:         "Utilisation des expressions régulières"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Les expressions régulières (regex) filtrent et manipulent les textes selon des motifs définis. C'est puissant pour valider, extraire ou remplacer des chaînes de caractères rapidement et efficacement.

## Comment ça marche ?

```Go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    // Exemple de validation de motif e-mail simple
    regexEmail, _ := regexp.Compile(`^[a-z0-9._%+\-]+@[a-z0-9.\-]+\.[a-z]{2,4}$`)
    email := "exemple@domaine.fr"
    fmt.Println("E-mail valide ? ", regexEmail.MatchString(email))

    // Extrait tous les numéros dans une chaîne (pattern \d+ pour les chiffres)
    regexNumbers, _ := regexp.Compile(`\d+`)
    text := "Le prix est de 100 euros."
    nums := regexNumbers.FindAllString(text, -1)
    fmt.Println("Numéros trouvés : ", nums)

    // Remplacement de texte
    regexReplace, _ := regexp.Compile(`\bJava\b`)
    replacedText := regexReplace.ReplaceAllString("Java est fun. JavaScript est différent de Java.", "Go")
    fmt.Println("Texte après remplacement : ", replacedText)
}
```

Output:
```
E-mail valide ?  true
Numéros trouvés :  [100]
Texte après remplacement :  Go est fun. JavaScript est différent de Go.
```

## Plongée en profondeur

Regex existait avant Go : Perl et Python l'ont popularisé. Alternatives en Go : packages comme `bytes` et `strings` pour certaines tâches, mais regex est plus universel et souple. Implementation : la librairie `regexp` de Go utilise RE2, évitant les catastrophes de performances des expressions régulières "exponentielles".

## Voir aussi

- Go documentation officielle pour regexp : https://golang.org/pkg/regexp/
- Tutoriel interactif pour apprendre regex : https://regexone.com/
- Outil en ligne pour tester des expressions régulières : https://regex101.com/
