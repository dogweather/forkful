---
date: 2024-01-26 04:14:07.624169-07:00
description: "REPL, ou Boucle Lire-\xC9valuer-Imprimer, est un environnement de programmation\
  \ interactif qui prend des entr\xE9es utilisateur individuelles, les ex\xE9cute\
  \ et\u2026"
lastmod: '2024-03-13T22:44:58.326241-06:00'
model: gpt-4-0125-preview
summary: "REPL, ou Boucle Lire-\xC9valuer-Imprimer, est un environnement de programmation\
  \ interactif qui prend des entr\xE9es utilisateur individuelles, les ex\xE9cute\
  \ et retourne le r\xE9sultat."
title: Utilisation d'une console interactive (REPL)
weight: 34
---

## Quoi & Pourquoi ?
REPL, ou Boucle Lire-Évaluer-Imprimer, est un environnement de programmation interactif qui prend des entrées utilisateur individuelles, les exécute et retourne le résultat. Les programmeurs l'utilisent pour obtenir des retours instantanés, pour le débogage, et pour l'expérimentation rapide avec des concepts de codage sans la charge de compiler et exécuter un programme complet.

## Comment faire :
Dans Fish, le shell interactif est le mode par défaut lorsque vous le démarrez. Voici à quoi cela ressemble en action :

```Fish Shell
> set color blue
> echo "Le ciel est $color"
Le ciel est bleu
```

Vous pouvez également exécuter des fonctions intégrées et jouer avec les substitutions de commande :

```Fish Shell
> function cheer
      echo "Allez Fish $argv !"
  end
> cheer Coders
Allez Fish Coders !
```

Non seulement vous pouvez définir des fonctions, mais vous pouvez également exécuter des extraits de code à la volée et voir le résultat instantanément :

```Fish Shell
> math "40 / 2"
20
```

## Plongée profonde
Le concept de REPL remonte au langage de programmation Lisp dans les années 1960. Cette forme de programmation interactive a établi le benchmark pour des environnements comme `ipython` de Python et `irb` de Ruby. Fish continue la tendance avec un accent sur la convivialité et l'utilisation interactive.

Fish se différencie d'autres shells comme Bash en ce qu'il est conçu avec l'interactivité à l'esprit dès le départ. Il fournit une coloration syntaxique, des autosuggestions, et des complétions de tabulation qui le rendent puissant à utiliser dans un workflow de style REPL. Mieux encore, vos commandes sont mémorisées et recherchables, rendant les tests répétés un jeu d'enfant.

Les alternatives au REPL de Fish pourraient être `bash` ou `zsh` lorsqu'ils sont associés à des extensions comme `bash-completion` ou `oh-my-zsh`, mais Fish tend à offrir une expérience plus riche dès le départ.

## Voir aussi :
- Documentation Fish : https://fishshell.com/docs/current/index.html
- Une comparaison intéressante entre Fish et d'autres shells : https://www.slant.co/versus/2209/3686/~fish_vs_bash
- Une plongée plus profonde dans les REPLs : https://en.wikipedia.org/wiki/Read–eval–print_loop
- Programmation interactive en Lisp, un regard historique : http://www.paulgraham.com/ilisp.html
