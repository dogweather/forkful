---
title:                "Générer des nombres aléatoires"
html_title:           "Elixir: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
La génération de nombres aléatoires est une fonction codée qui produit une suite de nombres sans motif apparent. Les programmeurs l'utilisent pour diverses raisons comme les simulations, les jeux et les tests.

## Comment faire:
Voici un cas simple de génération d'un nombre aléatoire dans Bash:
```Bash
# Un nombre aléatoire entre 0 et 32767
echo $RANDOM
```
Et pour générer un nombre aléatoire dans un intervalle spécifique, par exemple entre 1 et 100:
```Bash
# Un nombre aléatoire entre 1 et 100
echo $((RANDOM%100+1))
```

## Plus en détail
Historiquement, la génération de nombres aléatoires était basée sur des phénomènes physiques, comme le lancé de dés. Aujourd'hui, de nombreux langages de programmation offrent des moyens plus sophistiqués et prévisibles pour générer des nombres aléatoires. Pour alternatives, vous pouvez utiliser `openssl` ou `/dev/urandom` pour générer des nombres aléatoires de haute qualité. Cependant, keep in mind, la génération de nombres aléatoires dans Bash se fait via une fonction pseudo-aléatoire, ce qui signifie qu'elle n'est pas idéale pour les situations où une grande sécurité est requise.

## Pour aller plus loin
[Pseudo-random number generation in Bash](https://www.linuxjournal.com/content/pseudo-random-number-generation-bash)
[Generating random data in Bash with OpenSSL and /dev/urandom](https://www.howtoforge.com/tutorial/generating-random-data-in-linux-with-openssl-and-dev-urandom/)
[Bash $RANDOM built-in Bash function](https://tldp.org/LDP/abs/html/randomvar.html)