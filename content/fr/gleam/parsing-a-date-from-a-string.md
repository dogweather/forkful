---
title:                "Analyser une date à partir d'une chaîne"
html_title:           "Clojure: Analyser une date à partir d'une chaîne"
simple_title:         "Analyser une date à partir d'une chaîne"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et Pourquoi?
La conversion d'une date à partir d'une chaîne de caractères (string) est une opération courante en programmation qui permet de transformer une date écrite dans un format lisible par l'homme en un objet gérable par une machine. Cela est essentiel pour traiter les informations de date en provenance de diverses sources.

## Comment faire :
Voici un exemple simple sur comment traduire une date à partir d'une string en utilisant Gleam.

```Gleam
import gleam/date.{from_string}

fn main() {
  case from_string("2021-12-31") {
    Ok(date) -> 
        io.println(date)
    Error(err) -> 
        io.println("Erreur lors de la conversion de la date : ", err)
  }
}
```

Si tout se passe bien, vous devriez voir cette sortie :
```Gleam
Date(year: 2021, month: 12, day: 31)
```

## Analyse approfondie
Historiquement, il était courant que les dates soient exprimées en texte, que ce soit dans les fichiers ou les bases de données. De nos jours, de nombreuses applications nécessitent toujours la conversion des dates à partir de chaînes de caractères. 

Il y a bien sûr des alternatives à la fonction `from_string` de Gleam, comme le traitement manuel de la chaîne de caractères ou l'utilisation d'autres bibliothèques.

En ce qui concerne l'implémentation, `from_string` extrait les composants de la date (année, mois, jour) à partir de la chaîne donnée, puis crée un nouvel objet `Date`. Si la chaîne n'est pas dans le bon format ou si la date n'est pas valide, une erreur est renvoyée.

## Voir Aussi
- Documentation officielle de Gleam : https://gleam.run/documentation/
- Guide pour Gleam `from_string`: https://hexdocs.pm/gleam_stdlib/gleam/date/from_string.html
- Discussion relative aux implémentations de date en Gleam : https://github.com/gleam-lang/gleam/issues/842