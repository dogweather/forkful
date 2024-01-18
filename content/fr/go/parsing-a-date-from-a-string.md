---
title:                "Analyse d'une date à partir d'une chaîne de caractères"
html_title:           "Go: Analyse d'une date à partir d'une chaîne de caractères"
simple_title:         "Analyse d'une date à partir d'une chaîne de caractères"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?
Parser une date à partir d'une chaîne est un processus qui implique de convertir une date écrite sous forme de texte en un format exploitable par un ordinateur. Les programmeurs le font pour traiter les dates dans des applications telles que les calendriers, les réservations en ligne et les systèmes de gestion de base de données.

## Comment faire:
Utilisez la fonction `Parse` de la bibliothèque `time` pour convertir une chaîne de caractères en une date Go. Exemple:

```Go
layout := "02-01-2006"
str := "01-02-2021"
date, _ := time.Parse(layout, str)
fmt.Println(date)
```
Sortie: `2021-01-02 00:00:00 +0000 UTC`

Vous pouvez également spécifier un fuseau horaire dans la chaîne de sortie en utilisant le format `Mon Jan 2 15:04:05 -0700 MST 2006`. Exemple:

```Go
layout := "01/02/2006 15:04:05 -0700"
str := "01/02/2021 12:00:00 +0300"
date, _ := time.Parse(layout, str)
fmt.Println(date)
```
Sortie: `2021-01-02 12:00:00 +0300 +0300`

## Plongée en profondeur:
Parsing de dates à partir de chaînes était traditionnellement une tâche fastidieuse et compliquée, nécessitant la manipulation de multiples formats de date différents. Avec Go, la fonction `Parse` utilise un "squelette" de date spécifique (le format `Mon Jan 2 15:04:05 -0700 MST 2006`) pour indiquer à l'ordinateur comment interpréter la chaîne de date entrée. D'autres langages de programmation, tels que Python, ont également des méthodes spécifiques pour le parsing de dates, mais Go offre une bibliothèque complète et facile à utiliser pour le traitement de dates.

## À voir également:
- [Documentation de la bibliothèque `time` de Go](https://golang.org/pkg/time/)
- [Exemples de parsing de dates avec Go](https://programming.guide/go/time-formatting-parsing.html)
- [Autres alternatives pour le parsing de dates en Go](https://godoc.org/github.com/araddon/dateparse)