---
title:    "Haskell: Transformant une date en chaîne de caractères"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Pourquoi

La conversion d'une date en chaîne de caractères est une tâche courante en programmation, surtout lorsqu'il s'agit de manipuler et d'afficher des données temporelles. En utilisant Haskell, cela peut sembler intimidant au premier abord, mais c'est en fait une tâche assez simple. Dans cet article, nous allons expliquer pourquoi et comment convertir une date en chaîne de caractères en utilisant Haskell.

## Comment faire

Pour commencer, nous aurons besoin de quelques modules pour gérer les dates et les chaînes de caractères en Haskell. Dans l'exemple suivant, nous utiliserons le module "Data.Time" pour la gestion des dates et le module "Data.Text" pour les chaînes de caractères.

```Haskell
import Data.Time
import Data.Text
```

Maintenant, supposons que nous avons une date stockée dans une variable "date" de type Day (représentant une journée spécifique) et que nous voulons la convertir en une chaîne de caractères au format "dd/mm/yyyy". Voici le code pour accomplir cette tâche :

```Haskell
let dateStr = pack $ formatTime defaultTimeLocale "%d/%m/%Y" date
```

Dans ce code, nous utilisons la fonction "formatTime" pour formater la date dans le modèle souhaité. Nous utilisions également la fonction "pack" pour convertir la chaîne de caractères en utilisant Data.Text.

Voici un exemple de sortie pour une date du 1er janvier 2020 :

```Haskell
"01/01/2020"
```

## Plongée profonde

Il est important de noter que la fonction "formatTime" utilise un modèle de chaîne pour indiquer comment la date doit être formatée. Dans l'exemple ci-dessus, nous avons utilisé "%d/%m/%Y", mais il existe plusieurs autres modèles disponibles qui permettent de personnaliser davantage la sortie de la date.

De plus, dans certains cas, il peut être nécessaire de convertir la date directement en un type de données chaîne de caractères pour une utilisation ultérieure, plutôt que de l'envelopper dans une variable "Data.Type". Pour cela, nous pouvons utiliser la fonction "show" pour convertir la date en une chaîne de caractères :

```Haskell
let dateStr = show date
```

Cela affichera la date dans le format complet, par exemple :

```Haskell
"2020-01-01"
```

## Voir aussi

- [Documentation sur les modules Data.Time et Data.Text](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Liste complète des modèles de chaîne pour la fonction "formatTime"](https://hackage.haskell.org/package/time-1.8.0.7/docs/Data-Time-Format.html#t:Format)
- [Documentation sur la fonction "show" pour la conversion d'une date en chaîne de caractères](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:show)