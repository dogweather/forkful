---
title:    "Fish Shell: Convertir une date en chaîne de caractères"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Pourquoi

Convertissez une date en une chaîne de caractères peut sembler un détail insignifiant, mais cela peut être essentiel pour de nombreuses tâches de programmation. Que ce soit pour afficher la date dans un format spécifique ou pour comparer des dates, la conversion en une chaîne de caractères facilite grandement le travail avec les dates dans votre code.

## Comment faire

L'utilisation de la coquille de poisson (Fish Shell) rend la conversion d'une date en une chaîne de caractères simple et efficace. Voici un exemple de code utilisant la commande `date` pour obtenir la date actuelle et la convertir en chaîne de caractères :

```Fish Shell

# Obtenir la date actuelle et la stocker dans une variable
set current_date (date)

# Convertir la date en une chaîne de caractères avec le format souhaité
set date_string (date -f "%Y-%m-%d" $current_date)

# Afficher la date sous forme de chaîne de caractères
echo $date_string

```

La sortie de ce code sera un format de date personnalisable, tel que `2020-10-01`, au lieu du format typique `Jeu 01 Oct 2020`.

## Plongée en profondeur

Vous pouvez personnaliser davantage la conversion d'une date en utilisant une combinaison de commandes Fish Shell. Par exemple, si vous souhaitez inclure l'heure dans votre chaîne de caractères, vous pouvez utiliser la commande `strftime` pour spécifier le format de l'heure :

```Fish Shell

# Obtenir la date actuelle et la stocker dans une variable
set current_date (date)

# Utiliser strftime pour inclure l'heure dans le format
set date_string (strftime "%Y-%m-%d %H:%M:%S" $current_date)

# Afficher la date avec l'heure sous forme de chaîne de caractères
echo $date_string

```

La sortie de ce code sera un format de date et d'heure, tel que `2020-10-01 14:30:15`.

## Voir aussi

- Documentation sur la commande `date` en Fish Shell : https://fishshell.com/docs/current/cmds/date.html
- Guide de référence sur la commande `strftime` : https://www.gnu.org/software/libc/manual/html_node/Formatting-Calendar-Time.html#Formatting-Calendar-Time