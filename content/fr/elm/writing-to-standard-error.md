---
title:                "Elm: Écriture vers les erreurs standard"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

L'écriture vers l'erreur standard est un outil utile pour comprendre et résoudre les erreurs dans votre code Elm. Cela vous permet de voir précisément où une erreur a été déclenchée et quelles données ont été utilisées lors de son exécution.

## Comment faire

```Elm
import Debug exposing (crash)

goCrazy : Int -> Int
goCrazy x =
    if x < 10 then
        x + 5

    else
        crash "Nombre x doit être inférieur à 10!"
```
Sortie : `"Nombre x doit être inférieur à 10!"`

En ajoutant la fonction `crash` à votre code Elm, vous pouvez spécifier un message d'erreur personnalisé qui sera affiché dans la console lorsque votre programme atteint cette ligne de code.

## Plongée en profondeur

Il est important de noter que l'écriture vers l'erreur standard n'est pas la seule façon de gérer les erreurs dans votre code Elm. Il existe également des méthodes telles que la gestion d'erreur avec `Maybe` et `Result` ainsi que l'utilisation de `Debug.log` pour afficher des valeurs tout en déboguant.

Cependant, l'écriture vers l'erreur standard peut être particulièrement utile pour les débutants en Elm, car elle leur permet de mieux comprendre où se trouvent les erreurs et pourquoi elles se produisent. De plus, cela peut aider à éviter les erreurs courantes telles que les boucles infinies ou les valeurs manquantes.

## Voir aussi

- Documentation Elm sur l'écriture vers l'erreur standard
- Article sur la gestion d'erreur en Elm avec `Maybe` et `Result`
- Tutoriel sur l'utilisation de `Debug.log` en Elm pour déboguer votre code