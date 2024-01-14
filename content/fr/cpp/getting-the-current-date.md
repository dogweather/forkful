---
title:    "C++: Obtenir la date actuelle"
keywords: ["C++"]
---

{{< edit_this_page >}}

# Pourquoi

Saviez-vous que l'une des tâches les plus courantes en programmation est de récupérer la date actuelle ? Que vous soyez un développeur débutant ou expérimenté, il est important de savoir comment récupérer la date en programmation C++. Cela peut sembler être une tâche simple, mais cela peut faire une grande différence dans vos projets. Dans cet article, nous allons découvrir pourquoi il est important de récupérer la date actuelle en C++.

# Comment faire

Pour récupérer la date actuelle en C++, nous allons utiliser la bibliothèque standard `chrono`. Cette bibliothèque fournit des classes et des fonctions pour la manipulation du temps.

Tout d'abord, nous allons inclure la bibliothèque `chrono`.

```C++
#include <chrono>
```

Ensuite, nous allons utiliser la fonction `std::chrono::system_clock::now()` pour récupérer la date et l'heure actuelles.

```C++
auto now = std::chrono::system_clock::now();
```

Nous pouvons ensuite utiliser la fonction `std::chrono::duration_cast` pour convertir la date en un type de données que nous pouvons afficher.

```C++
auto seconds = std::chrono::duration_cast<std::chrono::seconds>(now.time_since_epoch());
```

Et enfin, nous pouvons utiliser la fonction `std::chrono::system_clock::to_time_t` pour convertir la date en un format de date compréhensible pour l'utilisateur.

```C++
std::time_t current_time = std::chrono::system_clock::to_time_t(now);
```

Vous pouvez ensuite imprimer la date actuelle en utilisant la fonction `std::ctime`.

```C++
std::cout << "La date actuelle est : " << std::ctime(&current_time);
```

# Deep Dive

Maintenant que nous avons vu comment récupérer la date actuelle en C++, jetons un coup d'œil à certains détails techniques.

Tout d'abord, `chrono` utilise des horloges différentes pour les mesures du temps : `steady_clock`, `system_clock` et `high_resolution_clock`. `system_clock` est utilisée pour mesurer le temps sur le système, tandis que `steady_clock` est utilisée pour mesurer le temps écoulé de manière stable. `high_resolution_clock` utilise la mesure de temps la plus précise disponible sur le système.

Deuxièmement, `chrono` utilise des durées pour mesurer le temps, plutôt que des valeurs de date et d'heure. Les durées sont exprimées en nombre d'unités de temps, telles que des secondes, des millisecondes ou des microsecondes.

Enfin, `chrono` est basée sur un époque, qui est une date de référence à partir de laquelle les durées sont mesurées. Par défaut, l'époque utilisée est le 1er janvier 1970 à minuit UTC.

# Voir aussi

- [Documentation sur chrono en C++](https://en.cppreference.com/w/cpp/header/chrono)
- [Tutoriel sur chrono en C++](https://www.learncpp.com/cpp-tutorial/80-chrono-library/)
- [Article sur les horloges et les époques en C++](https://www.fluentcpp.com/2018/06/05/times-calendar-and-clocks-in-cpp/)