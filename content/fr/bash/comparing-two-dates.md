---
title:                "Comparaison de deux dates"
html_title:           "Bash: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Quoi & Pourquoi?

Comparer deux dates est une tâche courante en programmation, qui consiste à déterminer si une date est plus récente, plus ancienne ou égale à une autre date. Les programmeurs utilisent cette fonctionnalité pour effectuer des opérations de tri, de filtrage ou de vérification dans leurs programmes.

# Comment faire:

Voici deux façons simples de comparer des dates en Bash:

```Bash 
# Exemple 1: Comparaison de dates de manière numérique
if (( date1 > date2 )); then
  echo "date1 est plus récente que date2"
elif (( date1 < date2 )); then
  echo "date1 est plus ancienne que date2"
else
  echo "Les dates sont égales"
fi
```

```Bash 
# Exemple 2: Comparaison de dates en utilisant la commande date
date1=$(date -d "2021-01-01" +%s)
date2=$(date -d "2020-01-01" +%s)
if (( date1 > date2 )); then
  echo "date1 est plus récente que date2"
elif (( date1 < date2 )); then
  echo "date1 est plus ancienne que date2"
else
  echo "Les dates sont égales"
fi
```

Voici un exemple de sortie pour ces deux exemples:

```Bash 
date1 est plus récente que date2
```

```Bash 
Les dates sont égales
```

# Plongeon Profond:

Dans les versions précédentes de Bash, la comparaison de dates nécessitait l'utilisation d'options spécifiques pour la commande `date`. Cependant, depuis la version 4 de Bash, les expressions numériques ont été introduites, ce qui a facilité la comparaison de dates en utilisant des opérateurs tels que `>`, `<` et `==`.

Il existe également des alternatives telles que `awk` ou la commande `test`, qui peuvent également être utilisées pour comparer des dates en Bash.

Pour les développeurs plus expérimentés, il est possible de créer des fonctions personnalisées en utilisant des variables et des opérateurs de comparaison pour effectuer des comparaisons de dates plus complexes.

# Voir aussi:

- Documentation officielle de Bash pour plus d'informations sur les expressions numériques : <https://www.gnu.org/software/bash/manual/html_node/Conditional-Constructs.html#Conditional-Constructs>
- Tuto Bash sur les opérateurs de comparaison: <https://www.tutorialspoint.com/unix/awk_comparison_operators.htm>