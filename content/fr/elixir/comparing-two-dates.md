---
title:    "Elixir: Comparer deux dates"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation en général, ainsi que dans Elixir en particulier, il est souvent nécessaire de travailler avec des dates et de les comparer entre elles. Que ce soit pour trier des données, pour effectuer des opérations de calcul ou pour afficher des informations pour l'utilisateur, la comparaison de dates est un outil indispensable pour tout développeur.

## Comment faire

Pour comparer deux dates en Elixir, il existe plusieurs méthodes. La plus simple est d'utiliser la fonction `Date.compare/2`, qui prend en paramètres deux dates et renvoie un résultat indiquant si la première date est antérieure, égale ou ultérieure à la seconde. Voyons un exemple concret :

```
Elixir
date1 = ~D[2021-01-01]
date2 = ~D[2021-05-05]
Date.compare(date1, date2)
```

Le résultat de cette opération sera `-1`, signifiant que `date1` est antérieure à `date2`. Bien entendu, il est également possible d'utiliser d'autres fonctions telles que `Date.before?/2` ou `Date.after?/2` en fonction des besoins.

## Plongée en profondeur

Saviez-vous qu'en plus des fonctions mentionnées ci-dessus, Elixir offre également des opérateurs pour comparer des dates ? En effet, grâce aux modules `Date` et `DateTime`, il est possible d'utiliser les opérateurs `===`, `==`, `>=`, `<=`, `>` et `<` pour comparer des dates et des instants dans le temps. Cela peut être particulièrement utile lorsque l'on travaille avec des données provenant de sources externes et qui peuvent être formatées différemment.

## Voir aussi

- [Documentation d'Elixir sur la comparaison de dates](https://hexdocs.pm/elixir/Date.html#compare/2)
- [Article sur la manipulation de dates en Elixir](https://www.pandala1990.com/posts/manipulating-dates-times-in-elixir/)
- [Tutoriel sur les opérateurs en Elixir](https://elixir-lang.org/getting-started/operators.html)