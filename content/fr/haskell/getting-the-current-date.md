---
title:    "Haskell: Obtenir la date actuelle"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# Pourquoi

La date courante est une information essentielle pour de nombreuses applications, allant de la gestion des tâches à la création de journaux de bord. En utilisant Haskell, vous pouvez facilement obtenir la date courante et l'utiliser dans vos programmes de manière transparente.

# Comment faire

Pour obtenir la date courante en Haskell, nous pouvons utiliser la fonction ``getCurrentTime`` du module ``Data.Time``. Voici un exemple de code qui affiche la date courante au format ISO-8601 :

```Haskell
import Data.Time

main = do
  heureCourante <- getCurrentTime
  print heureCourante
```

En exécutant ce code, vous devriez obtenir une sortie similaire à ceci :

```
2019-09-30 15:25:03.561713 UTC
```

Nous pouvons également formater la date en utilisant la fonction ``formatTime``. Voici un exemple de code qui formate la date au format "jour/mois/année":

```Haskell
import Data.Time
import Data.Time.Format

main = do
  heureCourante <- getCurrentTime
  let format = "%d/%m/%Y" -- jour/mois/année
      heureFormatee = formatTime defaultTimeLocale format heureCourante
  print heureFormatee
```

La sortie devrait être similaire à ceci :

```
30/09/2019
```

# Plongée en profondeur

En utilisant la fonction ``getCurrentTime``, nous obtenons la date courante avec une précision d'une picoseconde. Cette précision peut sembler excessive pour certaines applications, c'est pourquoi le module ``Data.Time`` fournit également la fonction ``getCurrentTime``. Cette fonction fournit une date avec une précision d'une seconde. Vous pouvez choisir la fonction qui convient le mieux à vos besoins en fonction de la précision dont vous avez besoin.

De plus, en utilisant les fonctions ``nomJours`` et ``nomMois`` du module ``Data.Time.Calendar``, nous pouvons obtenir les noms des jours et des mois dans différentes langues. Par exemple, en utilisant ``nomMois francais``, nous pouvons obtenir les noms des mois en français.

# Voir aussi

- [Documentation du module Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Date et heure en Haskell](https://wiki.haskell.org/Date_and_time)
- [Code source des exemples](https://github.com/YourUsername/YourProject)