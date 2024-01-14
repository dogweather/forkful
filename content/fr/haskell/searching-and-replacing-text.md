---
title:                "Haskell: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

L'une des tâches courantes en programmation est le remplacement de texte. Que vous soyez débutant ou expérimenté en Haskell, il est important de savoir comment effectuer cette tâche de manière efficace et en utilisant les bonnes méthodes.

## Comment faire

Haskell offre plusieurs façons d'effectuer la recherche et le remplacement de texte dans une chaîne de caractères. La méthode la plus simple consiste à utiliser la fonction `substitute` de la bibliothèque `Data.String.Utils`. Voici un exemple de code montrant comment utiliser cette fonction :

```Haskell
import Data.String.Utils (replace)

main = do
  let phrase = "Bonjour tout le monde!"
  let nouvellePhrase = replace "Bonjour" "Salut" phrase
  print nouvellePhrase
```
Cet exemple remplace le mot "Bonjour" par "Salut" dans la phrase "Bonjour tout le monde!". Le résultat affiché sera "Salut tout le monde!".

Mais si vous voulez aller plus loin et remplacer toutes les occurrences d'un mot dans une phrase, vous pouvez utiliser les expressions régulières avec la bibliothèque `Text.Regex.Posix`. Voici un exemple de code montrant comment utiliser cette méthode :

```Haskell
import Text.Regex.Posix

main = do
  let phrase = "Bonjour tout le tout tout le monde!"
  let nouvellePhrase = subRegex (mkRegex "le") phrase "la"
  print nouvellePhrase
```
Dans cet exemple, nous avons remplacé toutes les occurrences du mot "le" par "la" dans la phrase donnée. Le résultat sera "Bonjour tout la tout tout la monde!".

## Plongée en profondeur

En Haskell, il existe plusieurs fonctions et bibliothèques dédiées à la recherche et au remplacement de texte, telles que `Data.Text`, `Data.ByteString`, etc. Chacune de ces bibliothèques a ses propres avantages et fonctionnalités, il est donc important de prendre le temps d'apprendre à les utiliser correctement.

De plus, il est également essentiel de comprendre le concept des expressions régulières et leur utilisation en Haskell. Les expressions régulières peuvent sembler complexes au début, mais une fois que vous les maîtriserez, elles deviendront un outil puissant pour le remplacement de texte.

## Voir aussi

Pour plus d'informations sur la recherche et le remplacement de texte en Haskell, vous pouvez consulter les liens suivants :

- [Documentation officielle de Haskell sur les expressions régulières](https://wiki.haskell.org/Regular_expressions)
- [Tutoriel sur les expressions régulières en Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/competition-winners/tobias-ostlund/parsing-a-simple-imperative-language)
- [Documentation officielle de la bibliothèque `Text.Regex.Posix`](https://hackage.haskell.org/package/regex-posix-0.95.2/docs/Text-Regular-Posix.html)