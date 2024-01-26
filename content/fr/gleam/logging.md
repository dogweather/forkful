---
title:                "Journalisation"
date:                  2024-01-26T01:03:09.585075-07:00
model:                 gpt-4-1106-preview
simple_title:         "Journalisation"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/logging.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
La journalisation est essentiellement la manière dont nous enregistrons ce qui se passe dans nos programmes. C'est comme avoir une petite boîte noire ; lorsque les choses tournent mal (et croyez-moi, cela arrivera), les journaux sont inestimables pour comprendre ce qui s'est passé, diagnostiquer les problèmes et optimiser les performances.

## Comment faire :
Dans Gleam, vous tireriez généralement parti d'une bibliothèque de journalisation — il n'existe pas de mécanisme de journalisation dédié prêt à l'emploi. Disons que nous utilisons une crate hypothétique `gleam_logger`. Voici comment vous pourriez l'intégrer :

```gleam
import gleam_logger

pub fn main() {
  gleam_logger.info("L'application démarre !")
  let result = intense_calculation()

  case result {
    Ok(value) -> 
      gleam_logger.debug("Calcul réussi", value)
    Error(err) -> 
      gleam_logger.error("Échec du calcul", err)
  }
}
```

Le résultat attendu dans vos journaux ressemblerait à ceci :

```
INFO: L'application démarre !
DEBUG: Calcul réussi 42
ERROR: Échec du calcul Raison : Division par zéro
```

## Plongée Profonde
L'art de la journalisation existe depuis les premiers jours de la programmation. Les opérateurs de système obtenaient littéralement des journaux de l'ordinateur - veillant à ce que tout fonctionne sans encombre. En accélérant, la journalisation est devenue numérique, devenant une partie centrale du développement logiciel.

Bien que Gleam, étant un langage relativement jeune qui cible l'écosystème Erlang, n'ait pas de cadre de journalisation intégré, vous pouvez tirer parti des installations de journalisation Erlang matures ou d'autres bibliothèques fournies par la communauté. Chacun offre des fonctionnalités et des compromis différents : certains peuvent fournir une journalisation structurée, d'autres sont plus pour une sortie de texte simple.

Maintenant, la question de la mise en œuvre d'une facilité de journalisation : Est-ce simple ? À première vue, oui. Mais en y regardant de plus près, vous vous trouvez à gérer la concurrence, les goulots d'étranglement I/O, la rotation des journaux, la standardisation des formats (pensez à JSON pour la journalisation structurée), le filtrage des niveaux et peut-être la traçabilité distribuée. De plus, dans un paradigme fonctionnel, vous voulez généralement que les effets secondaires (comme la journalisation) soient gérés de manière prévisible et contrôlée.

## Voir Aussi
Voici où vous pouvez trouver plus d'informations sur les tenants et les aboutissants de la journalisation dans Gleam et son écosystème environnant :
- [La documentation de :logger d'Erlang](http://erlang.org/doc/apps/kernel/logger_chapter.html) : Puisque Gleam se compile en Erlang, cela est directement applicable.
- [Les docs de la bibliothèque standard de Gleam](https://hexdocs.pm/gleam_stdlib/) : Pour des mises à jour sur les utilitaires de journalisation qui pourraient être ajoutés.
- [Awesome Gleam](https://github.com/gleam-lang/awesome-gleam) : Une liste organisée de ressources, qui pourrait inclure des bibliothèques de journalisation à mesure qu'elles deviennent disponibles.