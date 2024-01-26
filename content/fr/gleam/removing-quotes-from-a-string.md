---
title:                "Retirer les guillemets d'une chaîne"
date:                  2024-01-26T03:39:14.348454-07:00
model:                 gpt-4-0125-preview
simple_title:         "Retirer les guillemets d'une chaîne"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Retirer les guillemets d'une chaîne de caractères signifie enlever ces couches supplémentaires – les guillemets – de vos données textuelles. Les programmeurs font cela pour assainir l'entrée, préparer les chaînes pour le traitement ou juste pour garder les choses ordonnées et cohérentes dans leurs applications. Il s'agit en fin de compte de données propres et utilisables.

## Comment faire :
Supprimer les guillemets dans Gleam est simple. Nous pouvons utiliser la correspondance de motifs ou des fonctions de chaîne intégrées. Voici un exemple rapide pour illustrer :

```gleam
pub fn remove_quotes(text: String) -> String {
  let without_quotes = string.trim(text, "\"")
  without_quotes
}

pub fn main() {
  let text_with_quotes = "\"Bonjour, Monde !\""
  let cleaned_text = remove_quotes(text_with_quotes)
  io.println(cleaned_text)
}
```

Exemple de sortie :
```
Bonjour, Monde !
```

## Plongée Profonde
Historiquement, gérer les guillemets dans les chaînes de caractères a été une tâche courante dans le traitement de texte et les langages de script. En raison de la nature des chaînes de caractères, souvent étant une saisie utilisateur ou lues à partir de fichiers, elles peuvent venir avec des guillemets qui doivent être retirés pour diverses raisons, telles que l'insertion dans une base de données ou la mise en forme.

Dans Gleam, nous utilisons la fonction `string.trim` pour enlever les guillemets. Il existe des alternatives ! Nous pourrions parcourir la chaîne ou appliquer des expressions régulières, mais `string.trim` est votre outil pratique pour le travail en raison de sa brièveté et de sa performance.

Si nous plongeons dans les détails d'implémentation, `string.trim` fonctionne en retirant les caractères du début et de la fin de la chaîne qui correspondent au motif fourni. Donc, si vous avez des guillemets aux deux extrémités de votre chaîne, ils sont coupés d'un coup. Gardez à l'esprit qu'il supprime seulement les guillemets s'ils sont aux bords; les guillemets bien au milieu de votre texte resteront en place.

## Voir Aussi
Pour les esprits curieux qui veulent explorer plus :
- [Documentation du module String de Gleam](https://gleam.run/stdlib/string/)
- [Plus sur la correspondance de motifs dans Gleam](https://gleam.run/book/tour/pattern-matching)
- Discussions sur le traitement de texte en programmation sur [Stack Overflow](https://stackoverflow.com/questions/tagged/text-processing)