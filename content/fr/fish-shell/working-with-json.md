---
title:                "Manipulation de JSON"
html_title:           "Arduino: Manipulation de JSON"
simple_title:         "Manipulation de JSON"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
Travailler avec JSON, c'est jongler avec des données structurées – pensez configurations, API, et stockage. Les développeurs s'y collent pour échanger des données facilement entre différentes langues de programmation.

## How to:
Utiliser `jq` avec Fish pour manipuler du JSON:

```Fish Shell
# Installez jq
sudo apt-get install jq

# Parsez un JSON simple et accédez aux champs
echo '{"nom":"Jean", "age": 25}' | jq '.nom'
```

Sortie:
```
"Jean"
```

```Fish Shell
# Transformez JSON avec jq
echo '[{"nom":"Jean"}, {"nom":"Marie"}]' | jq 'map(.nom |= "Prénom: " + .)'
```

Sortie:
```
[
  {"nom": "Prénom: Jean"},
  {"nom": "Prénom: Marie"}
]
```

## Deep Dive
JSON, abréviation de JavaScript Object Notation, est né des besoins de JavaScript mais a conquis tous les langages. Alternatives? XML, trop lourd. YAML, pas mal pour la config. Pour l'implémentation, Fish n'analyse pas JSON nativement, d'où le recours à `jq`, un processeur JSON léger et puissant.

## See Also
- Documentation `jq`: https://stedolan.github.io/jq/manual/
- Guide JSON pour débutants: https://www.w3schools.com/whatis/whatis_json.asp
- Fish Shell Scripting Tutorial: https://fishshell.com/docs/current/tutorial.html
