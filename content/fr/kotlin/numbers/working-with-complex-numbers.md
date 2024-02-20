---
date: 2024-01-26 04:43:36.185998-07:00
description: "Les nombres complexes \xE9largissent notre syst\xE8me num\xE9rique pour\
  \ inclure les racines carr\xE9es des nombres n\xE9gatifs, o\xF9 l'unit\xE9 \"imaginaire\"\
  \ i \xE9quivaut \xE0 la\u2026"
lastmod: 2024-02-19 22:05:16.479166
model: gpt-4-0125-preview
summary: "Les nombres complexes \xE9largissent notre syst\xE8me num\xE9rique pour\
  \ inclure les racines carr\xE9es des nombres n\xE9gatifs, o\xF9 l'unit\xE9 \"imaginaire\"\
  \ i \xE9quivaut \xE0 la\u2026"
title: Manipulation des nombres complexes
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Les nombres complexes élargissent notre système numérique pour inclure les racines carrées des nombres négatifs, où l'unité "imaginaire" i équivaut à la racine carrée de -1. Les programmeurs les utilisent dans des domaines tels que l'ingénierie, la physique et le traitement du signal, parce qu'ils sont excellents pour modéliser les ondes, les oscillations et tout ce qui tourne.

## Comment faire :

Définissons une classe de nombre complexe de base en Kotlin :

```kotlin
data class Complex(val reel : Double, val imaginaire : Double) {
    operator fun plus(autre : Complex) = Complex(reel + autre.reel, imaginaire + autre.imaginaire)
    operator fun minus(autre : Complex) = Complex(reel - autre.reel, imaginaire - autre.imaginaire)
    operator fun times(autre: Complex) = Complex(
        reel * autre.reel - imaginaire * autre.imaginaire,
        reel * autre.imaginaire + imaginaire * autre.reel
    )
    
    override fun toString(): String = "($reel + ${imaginaire}i)"
}

fun main() {
    val a = Complex(1.0, 2.0)
    val b = Complex(3.0, 4.0)
    
    println("a + b = ${a + b}")  // Sortie : a + b = (4.0 + 6.0i)
    println("a - b = ${a - b}")  // Sortie : a - b = (-2.0 - 2.0i)
    println("a * b = ${a * b}")  // Sortie : a * b = (-5.0 + 10.0i)
}
```

## Plongée profonde

Les nombres complexes ont été mentionnés pour la première fois au 16e siècle, résolvant des équations cubiques qui manquaient de solutions réelles. L'ingénierie et la physique bénéficient grandement des nombres complexes pour analyser les circuits en courant alternatif et les formes d'onde. Vous pourriez alternativement utiliser une bibliothèque comme `koma` ou `ejml` de Kotlin pour un travail intensif.

Les opérations sur les nombres complexes reflètent les nombres réels, mais avec une attention à l'unité imaginaire. La multiplication, par exemple, suit la propriété distributive, en se rappelant que `i^2 = -1`. Cette unité imaginaire nous permet de représenter des nombres multidimensionnels, cruciaux dans divers calculs scientifiques.

## Voir aussi

Bibliothèques Math de Kotlin :

- [koma](https://koma.kyonifer.com/) : Une bibliothèque de calcul scientifique pour Kotlin.

Lecture supplémentaire sur les nombres complexes :

- [Wikipedia : Nombres complexes](https://fr.wikipedia.org/wiki/Nombre_complexe)
