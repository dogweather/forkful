---
title:                "Arrondir les nombres"
aliases:
- /fr/bash/rounding-numbers.md
date:                  2024-01-26T03:42:42.016457-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arrondir les nombres"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/rounding-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Arrondir les nombres signifie éliminer les décimales pour arriver à une valeur plus simple qui est suffisamment bonne pour un contexte donné. Les programmeurs arrondissent les nombres pour simplifier les résultats, économiser de l'espace ou parce que la valeur exacte n'est pas vitale - comme lorsque vous estimez visuellement l'utilisation du CPU ou l'espace disque, et que les décimales ne changeront pas votre journée.

## Comment :

Voici l'essentiel sur l'arrondissement dans Bash :

```Bash
# Arrondir à l'inférieur en utilisant 'floor' avec bc
echo "scale=0; 3.49/1" | bc

# Arrondir à supérieur en utilisant 'ceiling' avec bc
echo "scale=0; 3.01/1" | bc -l

# Arrondir au plus proche entier en utilisant printf
printf "%.0f\n" 3.49

# Une astuce pour arrondir au plus proche entier en utilisant bc
echo "(3.49+0.5)/1" | bc
```

Exemples de résultats — directement de la bouche du terminal :

```
3  # Arrondi à l'inférieur (floor)
4  # Arrondi à supérieur (ceiling)
3  # Arrondi au plus proche (avec printf)
3  # Arrondi au plus proche (avec bc)
```

## Approfondissement

Dans le temps, il n'y avait pas de `bc` ou `printf` dans les scripts Bash pour faire la magie des mathématiques. Les vieux de la vieille devaient compter sur des outils externes ou des solutions astucieuses. Maintenant, `bc` vous permet de faire des mathématiques de précision. Gardez à l'esprit que par défaut, `bc` n'arrondit pas — il prend l'inférieur. La partie scale définit l'action du point décimal.

Des alternatives ? Vous pourriez utiliser `awk` pour arrondir sans passer à `bc` ou vous débattre avec `perl` pour des besoins mathématiques plus lourds. Pour les masochistes, allez-y en pur Bash avec, disons, une manipulation de chaînes de caractères itérative – mais pourquoi faire ?

Quant aux détails, `bc` ne se limite pas à arrondir, il fait beaucoup de trucs en mathématiques — le mettre à l'échelle, le sinus, la racine carrée, vous l'appelez. Avec `printf`, il s'agit davantage de formater du texte, mais hé, ça arrondit les nombres, alors on ne se plaint pas.

## Voir Aussi

Pour ceux qui en veulent plus :

- Manuel de GNU `bc` : https://www.gnu.org/software/bc/manual/html_mono/bc.html
- Commande Bash `printf` : https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#index-printf
- Guide de l'utilisateur AWK (pour l'arrondissement et d'autres traitements de texte) : https://www.gnu.org/software/gawk/manual/gawk.html
- Plus de mathématiques, scripts et astuces numériques en Bash : https://mywiki.wooledge.org/BashFAQ/022
