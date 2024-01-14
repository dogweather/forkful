---
title:    "Fish Shell: Écrire des tests"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi

De nos jours, il est essentiel pour les programmeurs de s'assurer de la qualité et de la stabilité de leur code. Cela peut sembler fastidieux, mais en réalité, écrire des tests peut vous faire gagner du temps et éviter des problèmes à long terme. Avec Fish Shell, vous pouvez facilement écrire des tests pour votre code, et cet article va vous montrer comment.

## Comment faire

Pour écrire des tests avec Fish Shell, il suffit de créer un fichier avec l'extension .fish et d'utiliser la commande `test` pour effectuer les vérifications. Voici un exemple de code pour tester si une variable contient bien la valeur "bonjour" :

```Fish Shell
# Définition de la variable
set message "bonjour"

# Test pour voir si la variable contient la valeur "bonjour"
test $message = "bonjour"
```

Le résultat de ce test sera un code de sortie 0, ce qui signifie que le test a réussi. Si la variable n'avait pas la bonne valeur, le code de sortie serait 1, indiquant un échec.

Vous pouvez également utiliser la commande `not` pour effectuer des tests négatifs, comme dans cet exemple :

```Fish Shell
# Définition de la variable
set nombre 10

# Test pour voir si la variable n'est pas égale à 5
not test $nombre -eq 5
```

Encore une fois, le résultat de ce test sera un code de sortie 0, indiquant que le test a réussi.

## Deep Dive

Maintenant que vous savez comment écrire des tests de base avec Fish Shell, voici quelques éléments supplémentaires à prendre en compte :

- Vous pouvez également utiliser la commande `and` pour effectuer plusieurs tests en même temps. Le code de sortie sera 0 uniquement si tous les tests sont réussis.
- La commande `or` peut être utilisée pour effectuer des actions différentes selon le résultat du test (code de sortie 0 ou 1).
- Vous pouvez utiliser l'option `set -x` pour afficher les tests et leurs résultats dans la sortie standard.
- Il existe également des commandes plus spécifiques pour tester des conditions telles que `test -d` pour vérifier si un dossier existe ou encore `test -x` pour tester si un fichier est exécutable.

N'hésitez pas à consulter la documentation de Fish Shell pour plus d'informations et de détails sur l'écriture de tests.

## Voir aussi

- La documentation de Fish Shell sur l'utilisation des tests : [lien ici]
- Un tutoriel complet sur l'écriture de tests avec Fish Shell : [lien ici]
- Des astuces pour écrire des tests efficaces : [lien ici]