---
title:                "Interpolation de chaînes de caractères"
aliases:
- fr/fish-shell/interpolating-a-string.md
date:                  2024-01-20T17:50:35.027186-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolation de chaînes de caractères"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
L'interpolation de chaînes c'est insérer des variables ou des expressions dans une chaîne de caractères. On le fait pour dynamiser les textes et les rendre plus flexibles.

## How to:
En Fish, l'interpolation est simple et directe. Voici comment ça marche:

```Fish Shell
set nom "Monde"
echo "Bonjour, $nom!"
```

Sortie :

```
Bonjour, Monde!
```

Et avec des commandes :

```Fish Shell
set compteur (seq 3)
for nbr in $compteur
    echo "Compteur est à $nbr"
end
```

Sortie :

```
Compteur est à 1
Compteur est à 2
Compteur est à 3
```

## Deep Dive
Historiquement, Fish s'est démarqué pour son approche conviviale et son souci de simplification. Là où les autres shells utilisent des guillemets ou des caractères d'échappement, Fish se contente d'une syntaxe épurée.

Alternatives : Bash requiert souvent des guillemets ou `$()` pour des opérations similaires. Fish, lui, évite ces complications.

Détails d'implémentation : Fish utilise une expansion des variables en temps réel, ce qui signifie que la valeur d'une variable est évaluée et insérée lors de l'exécution de la commande.

## See Also
Pour plus d'informations, consultez la documentation officielle de Fish sur l'interpolation de chaînes et la manipulation de variables :

- Documentation Fish sur les variables : [https://fishshell.com/docs/current/#variables](https://fishshell.com/docs/current/#variables)
- FAQ Fish Shell : [https://fishshell.com/docs/current/faq.html#faq-string](https://fishshell.com/docs/current/faq.html#faq-string)
