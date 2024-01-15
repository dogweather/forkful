---
title:                "Comparaison de deux dates"
html_title:           "C++: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Comparer deux dates peut être utile lorsque vous travaillez avec des données temporelles dans votre programme. Cela peut vous aider à déterminer si une date est antérieure, postérieure ou égale à une autre date. Comparer des dates peut également être utile pour trier des données chronologiquement dans votre programme.

## Comment faire

Pour comparer deux dates en C++, vous pouvez utiliser la classe `std::chrono::system_clock` et la fonction `time_since_epoch()` pour obtenir le temps écoulé depuis le 1er janvier 1970 à minuit (également appelé "epoch"). Voici un exemple de code qui compare deux dates en utilisant cette méthode :

```C++
#include <iostream>
#include <chrono>

int main() {
	// Obtenez la date actuelle
	std::chrono::system_clock::time_point today = std::chrono::system_clock::now();

	// Obtenez la date du 1er janvier 2019
	std::chrono::system_clock::time_point newYear( 
		std::chrono::hours(24 * 365 + 5));

	// Comparez les deux dates en utilisant la fonction time_since_epoch()
	if (today.time_since_epoch() > newYear.time_since_epoch()) {
		std::cout << "Aujourd'hui est après le 1er janvier 2019." << std::endl;
	} else if (today.time_since_epoch() < newYear.time_since_epoch()) {
		std::cout << "Aujourd'hui est avant le 1er janvier 2019." << std::endl;
	} else {
		std::cout << "Aujourd'hui est le 1er janvier 2019!" << std::endl;
	}
	
	return 0;
}
```

La sortie de ce programme sera "Aujourd'hui est après le 1er janvier 2019." car la date actuelle est après le 1er janvier 2019.

## Plongée profonde

En C++, il existe plusieurs façons de comparer des dates, en utilisant différentes classes et fonctions telles que `std::chrono::steady_clock`, `std::time_t` et `std::tm`. Ces différentes options peuvent être utiles en fonction du type de comparaison de dates que vous souhaitez effectuer.

Il est également important de comprendre la notion de résolution de temps en C++. Par exemple, la classe `std::chrono::system_clock` a une résolution minimale d'une seconde, ce qui signifie que toutes les dates seront arrondies à la seconde près lorsqu'elles sont comparées. Si vous avez besoin d'une résolution plus précise, vous pouvez utiliser une classe avec une résolution plus fine comme `std::chrono::high_resolution_clock`.

## Voir aussi

- [Documentation officielle de C++ sur la gestion du temps](https://en.cppreference.com/w/cpp/chrono)
- [Guide pour travailler avec les dates en C++](https://medium.com/@jaceklaskowski/c-zones-for-dates-dealing-with-c-chrono-in-c-b04b9d22f9f7) 
- [Différentes méthodes pour comparer des dates en C++](https://www.techiedelight.com/compare-dates-cpp/)