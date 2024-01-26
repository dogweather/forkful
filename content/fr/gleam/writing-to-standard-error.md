---
title:                "Écrire dans l'erreur standard"
html_title:           "Arduino: Écrire dans l'erreur standard"
simple_title:         "Écrire dans l'erreur standard"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Écrire sur l'erreur standard (stderr) permet de séparer les messages d'erreur du flux de sortie principal (stdout). Les développeurs utilisent cela pour communiquer des problèmes sans perturber les données de sortie attendues.

## Comment faire :
```gleam
import gleam/io

pub fn main() {
  io.println("Affichage à l'écran") // Sortie standard
  io.eprint("Erreur !") // Erreur standard
}
```
Sortie attendue :
```
Affichage à l'écran
```
Sortie d'erreur (souvent visible dans la console) :
```
Erreur !
```

## Plongée en profondeur
Historiquement, séparer stdout et stderr permettait de traiter ou de rediriger les erreurs indépendamment. Des alternatives, comme les logs, existent mais n'offrent pas la même simplicité pour le débogage rapide. L'erreur standard dans Gleam utilise les mêmes principes sous-jacents que d'autres langages, reposant sur des conventions du système d'exploitation.

## Voir aussi
- Documentation Gleam sur les IO : https://hexdocs.pm/gleam_stdlib/gleam/io/
- La page Wikipedia sur les streams standards : https://fr.wikipedia.org/wiki/Flux_standard
- Un tutoriel sur la manipulation des sorties en Gleam : https://gleam.run/book/tour/io.html
