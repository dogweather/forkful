---
title:                "Rédaction de tests"
date:                  2024-01-19
html_title:           "Arduino: Rédaction de tests"
simple_title:         "Rédaction de tests"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Écrire des tests, c'est s'assurer que notre code fait ce qu'il doit faire. Les dev le font pour éviter des bugs, gagner du temps et dormir tranquilles.

## How to:
Créer un script de test simple en bash. Disons que t'as une fonc `addition()` dans `maths.sh`:

```bash
# maths.sh
addition() {
  echo $(($1 + $2))
}
```

Voilà le test dans `test_maths.sh`:

```bash
# test_maths.sh
source maths.sh
result=$(addition 2 3)

if [ $result -eq 5 ]; then
  echo "Ça marche!"
else
  echo "Houston, on a un problème..."
fi
```

Lance ton test:

```bash
bash test_maths.sh
```

Sortie attendue:

```
Ça marche!
```

## Deep Dive
À l'origine, tests en Bash étaient rudimentaires, genre vérifier le code retour d'un prog. Aujourd'hui, t'as des frameworks de test, comme shUnit2 et Bats, qui simplifient la vie. Avec ces outils, tu peux mocker des foncs, et même tester des scripts interactifs.

## See Also
- shUnit2 : [https://github.com/kward/shunit2](https://github.com/kward/shunit2)
- Bats : [https://github.com/bats-core/bats-core](https://github.com/bats-core/bats-core)
- Guide Bash avancé : [https://tldp.org/LDP/abs/html/](https://tldp.org/LDP/abs/html/)
