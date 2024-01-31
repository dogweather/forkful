---
title:                "Affichage des sorties de débogage"
date:                  2024-01-20T17:52:27.444545-07:00
model:                 gpt-4-1106-preview
simple_title:         "Affichage des sorties de débogage"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Imprimer les messages de débogage, c'est comme laisser des miettes de pain pour retracer votre chemin dans le code. Les développeurs le font pour comprendre comment le programme se comporte en exécutant.

## How to (Comment faire)
Go offre le package `fmt` pour tout ce qui touche à l'impression.

```Go
package main

import (
	"fmt"
	"log"
)

func main() {
	// Basique
	fmt.Println("Débogage : Je suis ici.")

	// Formaté
	name := "Gopher"
	fmt.Printf("Salut, %s! Vérifions comment tu vas.\n", name)

	// Avec log
	log.Println("Hmm, ceci est un message de log du débogage.")
}
```

Sortie typique :

```
Débogage : Je suis ici.
Salut, Gopher! Vérifions comment tu vas.
2023/01/02 15:04:05 Hmm, ceci est un message de log du débogage.
```

## Deep Dive (Plongée en profondeur)
Autrefois, les développeurs plantaient leur drapeau avec des `printf` partout. Ce bon vieux temps! Aujourd'hui, on veut gérer ça proprement. Go offre `fmt` pour imprimer vite et bien, mais pour plus de sophistication, il y a `log`, qui date et trace les messages.

Des alternatives ? Bien sûr. Utilisez `logrus` ou `zap` pour des besoins complexes, comme structurer les journaux ou gérer les niveaux de log.

Du côté de l'implémentation, `fmt` travaille avec des interfaces, pas que des chaînes. Si quelque chose a une méthode `String() string`, `fmt` sait l'imprimer. C'est malin, quand on y pense.

## See Also (Voir Aussi)
- Documentation `fmt`: https://pkg.go.dev/fmt
- Documentation `log`: https://pkg.go.dev/log
- Logrus: https://github.com/sirupsen/logrus
- Zap: https://github.com/uber-go/zap
