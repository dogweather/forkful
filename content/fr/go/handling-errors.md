---
title:                "Gestion des erreurs"
date:                  2024-01-26T00:52:48.799728-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gestion des erreurs"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/handling-errors.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

La gestion des erreurs en Go consiste à intercepter et à répondre avec élégance aux problèmes d'exécution. Nous le faisons pour éviter les plantages et garantir que nos programmes se comportent de manière prévisible, même lorsque les choses tournent mal.

## Comment faire :

Go utilise une gestion explicite des erreurs. Cela signifie que vous vérifierez si une fonction retourne une erreur chaque fois que vous l'appelez. Pas d'exceptions. Voici à quoi cela ressemble :

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	err := doSomething()
	if err != nil {
		fmt.Println("Aïe :", err)
		os.Exit(1)
	}
}

func doSomething() error {
	// Faisons semblant que quelque chose a mal tourné
	return fmt.Errorf("quelque chose a mal tourné")
}
```

Exécutez cela, et vous obtiendrez :

```
Aïe : quelque chose a mal tourné
```

Mais que se passe-t-il si cela réussit ?

```Go
func doSomething() error {
	// Tout va bien cette fois
	return nil
}
```

Pas de sortie. Cool, pas de nouvelles, bonne nouvelle.

## Plongée en profondeur :

En Go, la gestion des erreurs a été un point de discorde. Depuis le début, Go a choisi de ne pas utiliser les exceptions au profit d'une approche plus explicite, que certains développeurs apprécient pour sa simplicité mais que d'autres trouvent verbeuse. Le type intégré `error` est une interface. Tout type ayant une méthode `Error() string` le satisfait. Cela s'inscrit dans l'éthique de Go en matière de simplicité et d'explicité.

Des alternatives ? Il y a le duo `panic` et `recover`, mais ils sont réservés à des cas exceptionnels (jeu de mots) lorsque le programme ne peut pas continuer. Pensez à `panic` comme le bouton d'éjection que vous appuyez lorsque vous savez qu'il n'y a pas de retour en arrière possible. À utiliser avec parcimonie.

Quant à la gestion des erreurs grand public, Go 1.13 a introduit l'enveloppement des erreurs, facilitant la compréhension de la "chaîne d'erreurs" avec des fonctions comme `errors.Is()` et `errors.As()`.

## Voir aussi :

Pour tout ce qui concerne la gestion des erreurs en Go :

- Le blog Go sur la gestion des erreurs : [https://blog.golang.org/error-handling-and-go](https://blog.golang.org/error-handling-and-go)
- Effective Go – Section gestion des erreurs : [https://golang.org/doc/effective_go#errors](https://golang.org/doc/effective_go#errors)
- Documentation sur l'enveloppement des erreurs de Go 1.13 : [https://golang.org/doc/go1.13#error_wrapping](https://golang.org/doc/go1.13#error_wrapping)
- L'article de Dave Cheney sur les stratégies de gestion des erreurs : [https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully](https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully)
