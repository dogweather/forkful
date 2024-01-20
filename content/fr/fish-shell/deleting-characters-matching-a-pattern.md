---
title:                "Suppression de caractères correspondant à un motif"
html_title:           "C: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Supprimer des caractères correspondant à un motif (pattern matching) est une fonctionnalité permettant de traiter un groupe spécifique de caractères dans une séquence donnée. Les programmeurs le font pour gérer efficacement les données, surtout lorsqu'ils doivent exécuter des manipulations complexes de chaînes.

## Comment faire:

Voici un exemple concret sur la façon de supprimer des caractères qui correspondent à un motif en utilisant Fish Shell.

```fish shell
echo "Bonjour le monde" | string match -r -v 'o'
``` 
L’exemple ci-dessus supprime toutes les occurrences de 'o' dans la chaîne. Le drapeau `-r` habilite l'expression régulière et le drapeau `-v` inverse la correspondance des motifs, ce qui nous donne 'Bnjour le mnde' comme sortie.

## Approfondissement

Historiquement, le langage de programmation de shell, y compris Fish, a toujours été crucial pour le traitement des chaînes et la manipulation des données. La suppression des caractères correspondants à un motif est une fonction clé qui existe depuis longtemps dans les langages de scripts de shell. 

Parmi les alternatives, on peut utiliser sed, awk ou des langages de script comme Python ou Perl pour effectuer des opérations similaires, mais Fish Shell offre un moyen plus direct pour les utilisateurs de Linux / Unix.

De plus, l'implémentation de cette fonctionnalité utilise des expressions régulières, une technique puissante dans l'informatique pour la correspondance des motifs dans le texte. Dans le contexte actuel, elle est largement utilisée pour la conversion des données, la validation et le parsing.

## Voir Aussi 

Pour plus de renseignements sur l'utilisation de Fish Shell pour le traitement des chaînes, vous pouvez consulter les liens ci-dessous :

1. Documentation officielle de Fish Shell: [Link](https://fishshell.com/docs/current/index.html)
2. Guide de traitement des chaînes en Fish Shell: [Link](https://fishshell.com/docs/current/cmds/string.html)
3. Expression Régulière en Fish Shell: [Link](https://fishshell.com/docs/current/tutorial.html#tut_regexes)