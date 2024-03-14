---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:07:09.794660-07:00
description: "Retirer les guillemets d'une cha\xEEne en Go consiste \xE0 \xE9liminer\
  \ les guillemets de d\xE9but et de fin (`\"` ou `'`) d'une cha\xEEne donn\xE9e.\
  \ Les programmeurs ont\u2026"
lastmod: '2024-03-13T22:44:57.119667-06:00'
model: gpt-4-0125-preview
summary: "Retirer les guillemets d'une cha\xEEne en Go consiste \xE0 \xE9liminer les\
  \ guillemets de d\xE9but et de fin (`\"` ou `'`) d'une cha\xEEne donn\xE9e. Les\
  \ programmeurs ont\u2026"
title: "Supprimer les guillemets d'une cha\xEEne de caract\xE8res"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Retirer les guillemets d'une chaîne en Go consiste à éliminer les guillemets de début et de fin (`"` ou `'`) d'une chaîne donnée. Les programmeurs ont souvent besoin d'effectuer cette tâche pour assainir l'entrée utilisateur, analyser plus efficacement les données textuelles ou préparer les chaînes pour un traitement ultérieur qui nécessite un contenu sans guillemets.

## Comment faire :

Go propose plusieurs approches pour retirer les guillemets d'une chaîne, mais l'une des méthodes les plus simples est d'utiliser les fonctions `Trim` et `TrimFunc` fournies par le paquet `strings`. Voici comment procéder :

```go
package main

import (
	"fmt"
	"strings"
	"unicode"
)

func main() {
	quotedString := `"Ceci est une chaîne 'citée'"`

	// Utiliser strings.Trim pour retirer des guillemets spécifiques
	unquoted := strings.Trim(quotedString, `"'`)
	fmt.Println("En utilisant strings.Trim :", unquoted)

	// Approche personnalisée utilisant strings.TrimFunc pour plus de contrôle
	unquotedFunc := strings.TrimFunc(quotedString, func(r rune) bool {
		return r == '"' || r == '\''
	})
	fmt.Println("En utilisant strings.TrimFunc :", unquotedFunc)
}
```

Cet exemple montre deux approches pour retirer à la fois les guillemets doubles (`"`) et simples (`'`). La fonction `strings.Trim` est plus simple et fonctionne bien lorsque vous savez exactement quels caractères retirer. D'autre part, `strings.TrimFunc` offre plus de flexibilité, vous permettant de spécifier une fonction personnalisée pour décider quels caractères sont retirés. Le résultat de l'exemple de code ci-dessus est :

```
En utilisant strings.Trim : Ceci est une chaîne 'citée'
En utilisant strings.TrimFunc : Ceci est une chaîne 'citée'
```

Les deux méthodes permettent de retirer efficacement les guillemets de début et de fin de la chaîne.

## Approfondissement

Les fonctions `Trim` et `TrimFunc` du paquet `strings` font partie de la vaste bibliothèque standard de Go, conçue pour offrir des capacités de manipulation de chaînes puissantes, mais simples, sans avoir besoin de paquets tiers. Historiquement, la nécessité de manipuler et de traiter efficacement les chaînes découle de l'objectif principal de Go sur les serveurs réseau et les analyseurs de données, où le traitement des chaînes est une tâche courante.

Un aspect notable de ces fonctions est leur implémentation basée sur les runes (représentation par Go d'un point de code Unicode). Cette conception leur permet de gérer sans problème les chaînes contenant des caractères multi-octets, rendant l'approche de Go pour la manipulation des chaînes à la fois robuste et compatible avec Unicode.

Bien que l'utilisation directe de `Trim` et `TrimFunc` pour retirer les guillemets soit pratique et idiomatique en Go, il convient de mentionner que pour des tâches de traitement de chaînes plus complexes (par exemple, guillemets imbriqués, guillemets échappés), les expressions régulières (via le paquet `regexp`) ou l'analyse manuelle pourraient fournir de meilleures solutions. Cependant, ces alternatives s'accompagnent d'une complexité et de considérations de performance accrues. Par conséquent, pour la simple suppression de guillemets, les méthodes démontrées offrent un bon équilibre entre simplicité, performance et fonctionnalité.
