---
title:                "Elm: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous créez des applications en Elm, il est souvent nécessaire d'afficher la date actuelle à l'utilisateur. Cela peut sembler être une tâche simple, mais cela peut parfois être déroutant pour les débutants en programmation. Dans cet article, nous allons vous expliquer pourquoi il est important de savoir comment obtenir la date actuelle en Elm et comment le faire facilement.

## Comment Faire

La première étape pour obtenir la date actuelle en Elm consiste à importer le module Time dans votre code. Vous pouvez le faire en utilisant la commande suivante :

```
import Time
```

Ensuite, vous pouvez utiliser la fonction `now` pour obtenir la date actuelle sous forme de nombre de millisecondes depuis le 1er janvier 1970. Vous pouvez stocker cette valeur dans une variable et la transformer en une date lisible en utilisant la fonction `utcTimeToString`. Le code suivant illustre ce processus :

```
import Time

-- Obtenir la date actuelle en millisecondes
currentTime : Int
currentTime =
  Time.now

-- Transformer le nombre de millisecondes en une date lisible
currentDate : String 
currentDate =
  Time.utcTimeToString Time.utc currentTime
```

Si vous évaluez la variable `currentDate`, vous obtiendrez quelque chose comme `Tue, 16 Jul 2019 17:06:12 GMT`.

## Plongée en Profondeur

Maintenant que vous savez comment obtenir la date actuelle en Elm, il est également important de comprendre comment fonctionne la fonction `utcTimeToString`. Cette fonction prend deux arguments : un type Time et un nombre de millisecondes. Le type Time peut être soit `utc` pour l'heure universelle, soit `local` pour l'heure locale du système. En passant ces valeurs en paramètre, vous pouvez obtenir la date dans le fuseau horaire souhaité.

De plus, la fonction `utcTimeToString` accepte un troisième argument de type `DateTimeFormat` qui vous permet de personnaliser le format de la date. Par exemple, si vous voulez obtenir uniquement le jour et le mois de la date, vous pouvez utiliser `Time.CustomFormat "%a, %b %d"`.

## Voir Aussi

Pour plus d'informations sur la manipulation de dates en Elm, vous pouvez consulter les ressources suivantes :

- Le module Time dans la documentation Elm : https://package.elm-lang.org/packages/elm/time/latest/Time
- Un tutoriel sur la manipulation de dates en Elm : https://thoughtbot.com/blog/elm-and-dates

Maintenant que vous savez comment obtenir la date actuelle en Elm, vous pouvez facilement l'implémenter dans vos applications pour une meilleure expérience utilisateur. N'hésitez pas à explorer les différentes options de formatage de date pour répondre à vos besoins spécifiques. À bientôt pour un nouvel article sur la programmation en Elm !