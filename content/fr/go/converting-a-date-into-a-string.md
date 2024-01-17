---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Go: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Quoi & Pourquoi?
La conversion d'une date en chaîne de caractères est une tâche courante en programmation, qui consiste à transformer la représentation d'une date en un format lisible pour les humains. Les programmeurs font cela pour faciliter la communication avec les utilisateurs et pour afficher les dates correctement dans des interfaces graphiques ou des rapports.

# Comment faire:
Voici comment convertir une date en chaîne de caractères en utilisant Go:

```Go
// Définir la date à convertir
date := time.Date(2021, time.October, 31, 12, 0, 0, 0, time.UTC)

// Utiliser la fonction Format pour spécifier le format de la chaîne de caractères de sortie
strDate := date.Format("2 Jan 2006")

// Afficher la date sous forme de chaîne de caractères
fmt.Println(strDate)
```

Résultat:

```Go
31 Oct 2021
```

# Plongée en profondeur:
La nécessité de convertir des dates en chaînes de caractères est née de la différence entre les représentations de date utilisées par les machines et celles utilisées par les humains. Les programmeurs peuvent également utiliser des packages de formatage de dates tels que "strftime" pour réaliser la même tâche. La conversion des dates en chaînes de caractères peut être effectuée en utilisant la fonction Format de Go, qui prend en compte la syntaxe de format spécifique à Go pour les dates.

# Voir aussi:
Pour plus d'informations sur la conversion des dates en chaînes de caractères en utilisant Go, consultez la documentation officielle sur [time.Format](https://golang.org/pkg/time/#Time.Format) et [strftime](https://strftime.org/).