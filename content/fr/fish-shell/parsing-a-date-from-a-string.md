---
title:                "Analyser une date à partir d'une chaîne"
html_title:           "Clojure: Analyser une date à partir d'une chaîne"
simple_title:         "Analyser une date à partir d'une chaîne"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ? 

La conversion d'une date d'une chaîne de caractères consiste à prendre une date exprimée en texte et à la convertir en un format que le logiciel peut utiliser. Les programmeurs effectuent cette opération pour pouvoir effectuer des calculs avec des dates, les comparer, les stocker de manière plus efficace, etc.

## Comment faire :

Pour parseur une date d'une chaîne de caractère en utilisant Fish Shell, vous devez utiliser la commande `date` avec l'option `-d`. Voici un exemple:

```Fish Shell
# Parse une date à partir d'une chaîne de caractères
set date_string "2022-03-01 10:30:00"
date -d "$date_string" "+%Y-%m-%d %H:%M:%S"
```

L'exécution de ce script renverra

```
2022-03-01 10:30:00
```

## Plongée en profondeur :

Historiquement, le parseur d'une date d'une chaîne de caractères était une tâche complexe du fait des différents formats de date existants. Aujourd'hui, la majorité des langages de programmation et des interpréteurs de commandes, comme Fish Shell, offrent un moyen direct pour le faire.

En alternative à Fish Shell, il existe d'autres interpréteurs de commandes qui permettent de parser une date, comme Bash et Zsh, avec leur propre syntaxe.

En ce qui concerne les détails de l'implémentation, l’interpréteur de commandes Fish Shell utilise la fonction `strptime()` de la bibliothèque C pour faire le parsing.

## Voir aussi :

Pour plus d'informations sur le Shell de poisson, consultez le [tutoriel officiel](https://fishshell.com/docs/current/tutorial.html) et le [manuel du langage de script](https://fishshell.com/docs/current/language.html).