---
title:                "Refactoring"
aliases:
- fr/fish-shell/refactoring.md
date:                  2024-01-26T01:18:04.666224-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/refactoring.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
La refactorisation est le processus de restructuration du code existant sans changer son comportement externe afin d'améliorer les attributs non fonctionnels. Les programmeurs le font pour rendre le code plus lisible, réduire la complexité, améliorer la maintenabilité, et faciliter l'évolution ou la modification ultérieure.

## Comment faire :
Imaginez que vous avez un script qui a beaucoup grandi avec le temps. Il a commencé simplement, mais maintenant, c'est une bête tentaculaire avec des tentacules de logique. Voici un exemple simple de refactorisation d'une fonction pour la rendre plus lisible et efficace :

Avant la refactorisation :
```fish
function old_and_clunky
    set color (cat ~/.config/fish/color_theme)
    if test "$color" = 'blue'
        echo 'Thème bleu défini !'
    else if test "$color" = 'red'
        echo 'Thème rouge défini !'
    else
        echo 'Thème par défaut défini !'
    end
end
```

Après la refactorisation :
```fish
function set_theme_color
    set theme_color (cat ~/.config/fish/color_theme)
    switch $theme_color
        case blue
            echo 'Thème bleu défini !'
        case red
            echo 'Thème rouge défini !'
        default
            echo 'Thème par défaut défini !'
    end
end
```
La refactorisation a amélioré le nom de la fonction pour mieux décrire son but et a remplacé la chaîne de if-else par une instruction `switch` plus propre.

Exemple de sortie :
```
Thème bleu défini !
```

## Approfondissement
La refactorisation a été décrite en détail pour la première fois dans le livre séminal de Martin Fowler "Refactoring: Improving the Design of Existing Code". Le livre a établi une approche structurée pour améliorer le code sans écrire de nouvelles fonctionnalités. De nombreuses techniques de refactorisation ont été introduites depuis lors, et le concept est devenu une partie fondamentale du développement logiciel moderne.

Dans l'environnement Fish Shell, la refactorisation peut sembler légèrement différente que dans d'autres contextes de programmation en raison de sa syntaxe spécialisée et de sa nature en ligne de commande. Les alternatives à la refactorisation de scripts dans Fish pourraient impliquer leur portage vers un autre langage de shell ou l'utilisation d'outils externes pour une gestion plus avancée des scripts. Cependant, conserver la syntaxe native de Fish signifie souvent une meilleure intégration avec les fonctionnalités du shell et une expérience globalement plus fluide.

Lors de la refactorisation dans Fish Shell, vous avez principalement affaire à des fonctions et à des commandes par opposition à des classes ou modules de grande portée communs dans d'autres langages. Cette granularité peut rendre la tâche de refactorisation plus immédiate et directe, mais elle souligne également l'importance d'un code clair, concis et maintenable.

## Voir aussi
- Le site Web de Refactoring de Martin Fowler : [https://refactoring.com/](https://refactoring.com/)
- Documentation officielle de Fish Shell : [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
