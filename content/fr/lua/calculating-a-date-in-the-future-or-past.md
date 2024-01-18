---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "Lua: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi?

Calculer une date dans le futur ou dans le passé est une tâche courante pour les programmeurs. Cela consiste à trouver la date qui correspond à un certain nombre de jours, de semaines ou de mois avant ou après une date donnée. Cette fonctionnalité est utile dans de nombreux scénarios, tels que la planification d'événements ou la manipulation de données temporelles.

## Comment faire:

Dans Lua, il existe plusieurs façons de calculer une date dans le futur ou dans le passé. Voici quelques exemples de code qui montrent différentes approches pour y parvenir:

```
-- Exemple 1: Utilisation de la fonction os.date()

-- Calculer la date d'aujourd'hui
local today = os.date("*t")

-- Ajouter 7 jours à la date d'aujourd'hui
today.day = today.day + 7

-- Convertir la date en chaîne de caractères au format AAAA-MM-JJ
local futureDate = os.date("%Y-%m-%d", os.time(today))

print(futureDate)
-- Output: 2021-11-02 (si la date d'aujourd'hui est le 26 octobre 2021)

```

```
-- Exemple 2: Utilisation de la fonction os.time()

-- Obtenir le nombre de secondes depuis le 1er janvier 1970
local timestamp = os.time()

-- Ajouter 2 mois à la date actuelle (calculé en secondes)
local futureTimestamp = timestamp + (60*60*24*31*2)

-- Convertir le nouveau timestamp en date au format AAAA-MM-JJ
local futureDate = os.date("%Y-%m-%d", futureTimestamp)

print(futureDate)
-- Output: 2022-01-26 (si la date actuelle est le 26 novembre 2021)

```

## Plongée en profondeur:

Historiquement, la représentation des dates a toujours été un défi pour les programmeurs. Avant l'introduction de l'horloge interne dans les ordinateurs, les dates étaient calculées en utilisant des formules complexes basées sur le calendrier grégorien. Heureusement, avec l'avancement de la technologie et l'utilisation de bibliothèques de fonctions telles thatla fonction `os.date()`, les programmeurs n'ont plus à s'inquiéter de ces calculs fastidieux.

Alternativement, vous pouvez également utiliser des bibliothèques spécifiques pour le traitement des données temporelles telles que [Lua Date](https://github.com/Tieske/date), qui offre une syntaxe plus conviviale pour la manipulation des dates.

Pour implémenter le calcul d'une date dans le futur ou dans le passé, il est important de comprendre les différentes unités de temps et comment les convertir les unes en les autres. En outre, il est également important de tenir compte des variations potentielles telles que les années bissextiles ou les fuseaux horaires.

## Voir aussi:

- [Documentation Lua sur la fonction os.date()](https://www.lua.org/manual/5.4/manual.html#6.9)
- [Calculer des dates en utilisant les opérateurs Lua](https://www.lua.org/pil/4.3.1.html)
- [Lua Date, une bibliothèque de traitement des dates en Lua](https://github.com/Tieske/date)