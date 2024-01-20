---
title:                "Démarrer un nouveau projet"
html_title:           "Elm: Démarrer un nouveau projet"
simple_title:         "Démarrer un nouveau projet"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Démarrer un nouveau projet c'est lancer un voyage dans la création de code neuf, conçu pour résoudre un problème spécifique. Les programmeurs le font pour innover, résoudre des problèmes, améliorer des outils existants ou tout simplement pour le plaisir de coder.

## Comment faire:

Pour commencer un nouveau projet Gleam, c'est très simple. Utilisez le code suivant:

```Gleam
rebar3 new gleam_lib mon_projet
```

Cela va créer un nouveau projet Gleam dans un dossier appelé "mon_projet". Vous pouvez alors écrire votre premier module avec la commande suivante:

```Gleam
pub fn bonjour() {
    "Bonjour le monde"
}
```

Et pour l'exécuter, utilisez:

```Gleam
rebar3 shell
1> mon_projet:bonjour().
"Bonjour le monde"
```

## Plongée en Profondeur:

Historiquement, commencer un nouveau projet a toujours été un moment dynamique pour un développeur. C'est une chance de partir de zéro, sans les contraintes des codes hérités. C'est l'occasion d'explorer de nouvelles approches, des technologies récentes, sans oublier de résoudre des problèmes nouveaux ou existants.

En termes d'alternatives, vous pourriez choisir d'étendre un projet existant ou de contribuer à un projet open-source. Cependant, commencer un nouveau projet présente l'avantage évident de pouvoir définir vos propres règles et structures, et de contrôler entièrement le processus de développement.

En outre, Gleam facilite le démarrage d'un nouveau projet grâce à son type système statique avec une inférence de type avancée, son langage propre et lisible, et son interopérabilité transparente avec la plateforme Erlang/OTP.

## Voir Aussi:

1. Documentation Gleam: https://hexdocs.pm/gleam/gleam_stdlib/
2. Code source Gleam: https://github.com/gleam-lang/gleam
3. Tutoriel Gleam: https://gleam.run/getting-started/
4. Blog Gleam: https://gleam.run/news/