---
title:                "Verwendung von assoziativen Arrays"
aliases:
- de/fish-shell/using-associative-arrays.md
date:                  2024-01-30T19:10:46.687779-07:00
model:                 gpt-4-0125-preview
simple_title:         "Verwendung von assoziativen Arrays"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Assoziative Arrays oder Hash-Maps ermöglichen es Ihnen, Daten als Schlüssel-Wert-Paare zu speichern, was die Organisation und das Abrufen von Informationen nach Schlüssel erleichtert. Sie sind praktisch, wenn Sie eine strukturiertere Möglichkeit benötigen, Daten zu handhaben als nur Listen, insbesondere bei Konfigurationen und beim Umgang mit verschiedenen Attributen.

## Wie zu:

Fish unterstützt nicht nativ assoziative Arrays wie Bash 4+, aber Sie können eine ähnliche Funktionalität erreichen, indem Sie eine Kombination aus Listen und String-Manipulation verwenden. So können Sie sie nachahmen:

Zuerst richten Sie "assoziative Array"-Elemente separat ein:

```Fish Shell
set food_color_apple "red"
set food_color_banana "yellow"
```

Um auf ein Element zuzugreifen, referenzieren Sie es direkt:

```Fish Shell
echo $food_color_apple
# Ausgabe: red
```

Wenn Sie über sie iterieren müssen, verwenden Sie eine for-Schleife unter Berücksichtigung einer Namenskonvention:

```Fish Shell
for food in apple banana
    echo $food_color_$food
end
# Ausgabe:
# red
# yellow
```

Für diejenigen, denen Bashs `${!array[@]}` fehlt, um alle Schlüssel zu erhalten, können Sie Schlüssel in einer separaten Liste speichern:

```Fish Shell
set food_keys apple banana

for key in $food_keys
    echo $key 'ist' $food_color_$key
end
# Ausgabe:
# apple ist red
# banana ist yellow
```

## Tiefergehend

Echte assoziative Arrays, wie in anderen Skriptsprachen, sind noch kein Teil von Fishs Ansatz. Der gezeigte Workaround nutzt Fishs String-Manipulation und Listenfähigkeiten, um eine pseudo-assoziative Array-Struktur zu erstellen. Obwohl es funktioniert, ist es nicht so sauber oder fehlerfrei, wie es die eingebaute Unterstützung für assoziative Arrays wäre. Andere Shells wie Bash und Zsh bieten eingebaute assoziative Array-Funktionalität, was zu einfacherem, lesbarem Code führt. Allerdings zielt die Designphilosophie von Fish auf Einfachheit und Benutzerfreundlichkeit ab, möglicherweise auf Kosten solcher Funktionen. Der Workaround erfüllt die meisten Bedürfnisse, aber halten Sie ein Auge auf die Entwicklung von Fish Shell – dessen Entwickler verbessern aktiv und fügen Funktionen basierend auf Community-Feedback hinzu.
