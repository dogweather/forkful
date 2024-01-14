---
title:    "Haskell: Convertir une date en chaîne de caractères"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Pourquoi

Il est fréquent dans la programmation de devoir convertir une date en une chaîne de caractères, par exemple pour afficher la date dans un format compréhensible pour les utilisateurs. Dans cet article, nous allons voir comment réaliser cette conversion en Haskell et plonger plus en profondeur dans le fonctionnement de cette opération.

## Comment faire

La conversion d'un type de données `Date` en une chaîne de caractères peut se faire en utilisant la fonction `show` en combinaison avec le format `%m/%d/%Y` pour une date au format mois/jour/année ou `%d/%m/%Y` pour une date au format jour/mois/année. Voici un exemple de code en Haskell :

```Haskell
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock (getCurrentTime)
import System.Locale (defaultTimeLocale)

main = do
    currentTime <- getCurrentTime
    let dateString = formatTime defaultTimeLocale "%m/%d/%Y" currentTime
    putStrLn dateString
```

Ceci produira une sortie telle que `04/17/2021` en fonction de la date actuelle.

## Plongée en profondeur

La fonction `show` utilise en fait sous-jacente la fonction `formatTime` du module `Data.Time.Format` pour convertir une date en une chaîne de caractères. La chaîne de format `%m/%d/%Y` spécifie que la date doit être affichée avec le mois en premier, suivi du jour et enfin de l'année. On peut également utiliser d'autres spécificateurs de format pour changer l'ordre ou le style de la date, tels que `%b` pour afficher le mois sous forme de nom court ou `%Y %A` pour afficher l'année suivie du jour de la semaine.

Il est intéressant de noter que la chaîne de format fournie à la fonction `formatTime` suit la même syntaxe que la fonction `strftime` en langage C, ce qui peut faciliter la transition pour les développeurs familiers avec ce langage.

## Voir aussi

- [Documentation officielle de la fonction `formatTime`](https://hackage.haskell.org/package/time/docs/Data-Time-Format.html#v:formatTime)
- [Tutoriel complet sur la manipulation des dates en Haskell](https://www.vex.net/~trebla/haskell/time.xhtml)