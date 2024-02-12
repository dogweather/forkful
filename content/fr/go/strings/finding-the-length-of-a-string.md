---
title:                "Trouver la longueur d'une chaîne"
aliases: - /fr/go/finding-the-length-of-a-string.md
date:                  2024-02-03T17:56:56.212131-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trouver la longueur d'une chaîne"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/finding-the-length-of-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Trouver la longueur d'une chaîne en Go consiste à déterminer le nombre de caractères qu'elle contient. Les programmeurs effectuent régulièrement cette opération pour manipuler efficacement les chaînes de caractères, que ce soit pour la validation, l'extraction de sous-chaînes ou simplement pour appliquer des contraintes aux entrées des utilisateurs.

## Comment faire :
En Go, les chaînes sont traitées comme des séquences d'octets immuables. Vous pouvez trouver la longueur d'une chaîne en utilisant la fonction intégrée `len()` qui retourne le nombre d'octets, pas nécessairement le nombre de caractères. Voici comment l'utiliser :

```go
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	// Utilisation de len() pour trouver la longueur en octets
	str := "Hello, 世界"
	byteLength := len(str)
	fmt.Println("Longueur en Octets:", byteLength) // Sortie : Longueur en Octets : 13

	// Pour obtenir précisément le nombre de caractères ou de runes dans une chaîne
	runeLength := utf8.RuneCountInString(str)
	fmt.Println("Longueur en Runes:", runeLength) // Sortie : Longueur en Runes : 9
}
```
La première méthode utilisant `len()` peut ne pas toujours donner le résultat attendu puisqu'elle compte des octets. Pour les chaînes contenant des caractères non-ASCII (comme "世界"), `RuneCountInString` du paquet `unicode/utf8` devrait être utilisé à la place pour compter précisément les points de code Unicode.

## Exploration approfondie
Avant Go 1, il n'y avait pas de distinction stricte pour traiter les chaînes comme des séquences d'octets versus des séquences de caractères. Après Go 1, l'adoption de UTF-8 comme schéma d'encodage standard pour les chaînes a nécessité des approches plus claires. La fonction `len()` fonctionne parfaitement pour les chaînes ASCII, où les caractères sont représentés par un seul octet. Cependant, à mesure que les applications Go devenaient plus globales, et que le besoin de supporter une pléthore de langues et de jeux de caractères grandissait, l'approche simpliste de `len()` a montré ses limites.

L'introduction et l'utilisation de `utf8.RuneCountInString()` répondent à ces limites en fournissant un moyen de compter les véritables caractères Unicode (runes dans la terminologie de Go). Cette méthode assure que le calcul de la longueur est indépendant des spécificités de l'encodage d'UTF-8, où les caractères peuvent s'étendre sur plusieurs octets.

Une approche alternative pour traverser et manipuler des chaînes, plus en accord avec l'éthos de concurrence et d'efficacité de Go, pourrait impliquer de traiter les chaînes comme des tranches de runes. Cependant, cette méthode nécessite une étape de conversion et ne résout pas instantanément toutes les subtilités de Unicode (par exemple, les caractères combinés).

En résumé, alors que `len()` convient pour la longueur en octets et est efficace pour le texte ASCII, `utf8.RuneCountInString()` est un choix plus fiable pour une application globalement compatible. Pourtant, les développeurs sont encouragés à comprendre les compromis en termes de performance et d'utilisation de la mémoire que ces choix impliquent.
