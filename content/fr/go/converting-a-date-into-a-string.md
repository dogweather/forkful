---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Gleam: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

La conversion d'une date en chaîne de caractères consiste à transformer une valeur de date en une chaîne de caractères lisible. Les programmeurs le font pour simplifier les communications, faciliter l'affichage de données et rendre les dates indépendantes du format de stockage.

## Comment faire:

Pour convertir une date en chaîne de caractères, on utilise principalement le package `time`. Voici une procédure simple sous Go :

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	t := time.Now()
	fmt.Println(t.Format("2006-01-02"))
}
```
Dans cet exemple, `t.Format("2006-01-02")` convertit l'heure actuelle en une chaîne de caractères de format `AAAA-MM-JJ`.

## Plongée profonde

Historiquement, Go n'était pas conçu pour les manipulations de dates complexe, d'où la simplicité de son package `time`. Concernant les alternatives, il est aussi possible de composer sa propre mise en forme :

```Go
t := time.Now()
fmt.Printf("%04d-%02d-%02d\n",t.Year(), t.Month(), t.Day())
```
Ce code permet également d'obtenir une date en format `AAAA-MM-JJ` mais donne plus de contrôle sur les détails de formatage.

En ce qui concerne les détails d'implémentation, il est crucial de se rappeler que Go est sensible aux fuseaux horaires. Si vous manipulez des dates et des heures qui doivent rester cohérentes à travers différents fuseaux horaires, prenez soin d'utiliser `time.UTC` ou de spécifier un fuseau horaire.

## Voir aussi 

Pour plus d'informations sur le formatting des dates et des heures en Go, consultez la documentation officielle : https://golang.org/pkg/time/ . Vous pouvez aussi découvrir plus d'exemples et d'explications sur le site Go by Example : https://gobyexample.com/time-formatting-parsing .