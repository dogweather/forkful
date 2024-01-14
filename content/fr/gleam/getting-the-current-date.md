---
title:    "Gleam: Obtenir la date actuelle"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Pourquoi? La récupération de la date actuelle est une tâche couramment utilisée dans la programmation pour afficher la date et l'heure actuelles dans une application ou un système.

## Comment Faire
Pour récupérer la date actuelle en Gleam, nous pouvons utiliser la fonction `DateTime.now()`. Cette fonction renvoie un enregistrement contenant un champ `date` pour la date et un champ `time` pour l'heure. Nous pouvons utiliser la fonction `format` pour formater la date selon notre préférence, par exemple en utilisant le code `%Y-%m-%d` pour afficher la date au format AAAA-MM-JJ.

```Gleam
let current_date = DateTime.now();

let formatted_date = current_date |> format("%Y-%m-%d");

pub fn main() {
  IO.print("La date actuelle est :", formatted_date);
}
```

La sortie de ce code sera "La date actuelle est : 2021-01-08".

## Plongée Profonde
La fonction `DateTime.now()` utilise le fuseau horaire par défaut du système sur lequel le code est exécuté. Cela peut être modifié en utilisant la fonction `with_timezone` avant d'appeler `format`. De plus, la fonction `DateTime.now()` peut également prendre un argument pour spécifier un fuseau horaire différent si nécessaire.

## Voir Aussi
- Documentation officielle de Gleam sur les dates et heures: https://gleam.run/lib/std/datetime.html
- Tutoriel sur l'utilisation de Gleam pour manipuler les dates et heures: http://www.codingbeard.co.uk/guide-to-gleam-episodes/episode-24-date-and-time