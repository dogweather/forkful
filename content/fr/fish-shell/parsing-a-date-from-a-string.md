---
title:                "Analyse d'une date à partir d'une chaîne de caractères"
date:                  2024-01-20T15:35:45.492872-07:00
html_title:           "Arduino: Analyse d'une date à partir d'une chaîne de caractères"
simple_title:         "Analyse d'une date à partir d'une chaîne de caractères"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? / Quoi et Pourquoi ?
Extraire une date d'une chaîne de caractères permet de comprendre et manipuler des informations temporelles. Les programmeurs le font pour traiter des dates dans différents formats et pour les intégrer dans des opérations logiques.

## How to / Comment faire :
```Fish Shell
# Exemple de parsing d'une date :
set date_string "2023-03-15 10:00:00"
set epoch_time (date -ud "$date_string" +"%s")
echo $epoch_time

# Sortie exemple :
1678872000
```

## Deep Dive / Plongée en Profondeur
Historiquement, le shell a toujours été un outil pour manipuler du texte, y compris des dates. Fish Shell n'est pas différent, mais il est plus convivial. D'autres outils comme `dateutils`, `GNU date` ou des langages de script plus lourds (comme Python avec `datetime`) peuvent également effectuer le parsing. Concernant l'implémentation, la commande `date` sous Fish utilise les mêmes principes que les autres shells, mais la syntaxe est souvent plus épurée.

## See Also / Voir Aussi
- [Fish Documentation](https://fishshell.com/docs/current/index.html)
- [GNU Coreutils `date`](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Unix StackExchange discussion on date parsing in the shell](https://unix.stackexchange.com/questions/tagged/date)
