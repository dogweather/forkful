---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:07.895198-07:00
description: "Un shell interactif, ou boucle lire-\xE9valuer-afficher (REPL, de l'anglais\
  \ Read-Eval-Print Loop), vous permet d'exp\xE9rimenter avec du code Go en temps\
  \ r\xE9el,\u2026"
lastmod: '2024-03-13T22:44:57.135231-06:00'
model: gpt-4-0125-preview
summary: "Un shell interactif, ou boucle lire-\xE9valuer-afficher (REPL, de l'anglais\
  \ Read-Eval-Print Loop), vous permet d'exp\xE9rimenter avec du code Go en temps\
  \ r\xE9el, ex\xE9cutant des commandes et obtenant des retours imm\xE9diats."
title: "Utilisation d'un interpr\xE9teur de commandes interactif (REPL)"
weight: 34
---

## Comment faire :
Bien que Go n'inclue pas de REPL intégré, la communauté a créé des outils comme `gore` pour combler cette lacune. D'abord, installez `gore` en exécutant :

```
$ go get -u github.com/motemen/gore
```

Une fois installé, lancez `gore` en tapant `gore` dans votre terminal :

```
$ gore
```

Vous devriez voir une invite prête à accepter les commandes Go. Essayons un exemple simple :

```
gore> :import fmt
gore> fmt.Println("Bonjour, Go REPL !")
```

Vous verriez la sortie suivante :

```
Bonjour, Go REPL !
```

Les variables et les définitions de fonction fonctionnent comme prévu. Vous pouvez déclarer une fonction :

```
gore> :import math
gore> areaCircle := func(rayon float64) float64 {
...> return math.Pi * rayon * rayon
...> }
gore> fmt.Println("Aire d'un cercle de rayon 4 :", areaCircle(4))
```

Et obtenir la sortie immédiatement :

```
Aire d'un cercle de rayon 4 : 50.26548245743669
```

## Plongée profonde :
Le concept d'un REPL est ancien, remontant aux machines Lisp des années 1960, fournissant une expérience de programmation interactive. Contrairement à des langages comme Python ou JavaScript, Go a été conçu sans REPL, se concentrant à la place sur les binaires compilés pour la performance et la simplicité. Cela reflète la philosophie de Go en matière de simplicité et sa conception pour un logiciel évolutif et maintenable.

Cependant, des outils comme `gore` ou `goplay` montrent la ressource de la communauté Go dans le comblement de cette lacune. Ces outils analysent dynamiquement le code Go et utilisent le paquet `go/eval` ou des mécanismes similaires pour l'exécuter en temps réel, bien qu'avec certaines limitations par rapport à un environnement REPL natif. Ces limitations découlent du système de types de Go et de son modèle de compilation, qui peuvent rendre l'évaluation à la volée difficile.

Bien que les environnements REPL soient exceptionnellement utiles pour l'éducation et les tests rapides, l'écosystème Go tend généralement vers des processus de compilation et d'exécution traditionnels pour la plupart des tâches de développement. Les IDEs et éditeurs supportant Go, comme Visual Studio Code ou GoLand, offrent des outils intégrés pour les tests et le débogage qui allègent en grande partie le besoin d'un REPL pour le développement professionnel.

Pour la programmation exploratoire, le prototypage ou l'apprentissage, cependant, des REPLs comme `gore` offrent une alternative précieuse, permettant aux programmeurs habitués aux REPLs dans d'autres langues de profiter d'une expérience similaire en Go.
