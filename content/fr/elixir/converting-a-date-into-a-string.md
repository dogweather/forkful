---
title:    "Elixir: Convertir une date en chaîne de caractères"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# Pourquoi

Il existe de nombreuses raisons pour lesquelles vous pourriez vouloir convertir une date en chaîne de caractères dans le langage Elixir. Cela peut faciliter la manipulation de dates pour les afficher dans un format spécifique, les enregistrer dans une base de données ou les utiliser dans des contextes de comparaison.

# Comment faire

Pour convertir une date en string en utilisant Elixir, vous pouvez utiliser la fonction `Calendar.format`. Voici un exemple de code pour convertir une date en chaîne de caractères dans le format "jour-mois-année" :

```Elixir
date = ~D[2021-03-22]
formatted_date = Calendar.format(date, "~d-~m-~Y")
IO.puts formatted_date
```

La sortie de ce code serait "22-03-2021". Vous pouvez également utiliser d'autres formats de date en utilisant les tokens spécifiques de `Calendar.format`.

# Plongée en profondeur

En Elixir, les dates sont représentées par des structures de données appelées `Date` et `DateTime`. La fonction `Calendar.format` utilise ces structures pour convertir une date en chaîne de caractères. Il est également possible de personnaliser les formats en utilisant des tokens plus spécifiques et en utilisant des options de localisation pour les langues et les calendriers différents.

# Voir aussi

- Documentation des dates en Elixir : https://hexdocs.pm/elixir/Calendar.html
- Guide des tokens de formatage des dates : https://hexdocs.pm/elixir/Calendar.html#module-tokens-for-formatting-dates
- Tutoriel sur la manipulation des dates avec Elixir : https://dev.to/duffryn/how-to-use-dates-in-elixir-37jp