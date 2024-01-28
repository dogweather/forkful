---
title:                "Utilisation d'un débogueur"
date:                  2024-01-26T03:49:01.073544-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation d'un débogueur"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/using-a-debugger.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Utiliser un débogueur, c'est en quelque sorte jouer les détectives dans votre code, à la recherche de bugs et à comprendre pourquoi les choses ne fonctionnent pas de manière fluide. Les programmeurs le font parce que, soyons réalistes, les bugs sont inévitables, et les écraser efficacement signifie rendre votre code opérationnel plus rapidement et de manière plus fiable.

## Comment faire :
Gleam s'appuie actuellement sur l'écosystème Erlang pour les outils, vous utiliserez donc typiquement des outils comme `rebar3`, `observer`, et `debugger`. Voici comment vous salir les mains avec le débogage :

```gleam
// Dans votre configuration rebar, assurez-vous d'inclure ces lignes pour inclure les infos de débogage :
{erl_opts, [debug_info]}.

// Lancez un shell Erlang avec votre application chargée
rebar3 shell

// À l'intérieur du shell, vous pouvez démarrer le débogueur
1> debugger:start().
```

Simple, n'est-ce pas ? L'interface graphique du `débogueur` apparaît, et vous pouvez placer des points d'arrêt, parcourir le code pas à pas et surveiller les variables à loisir. Vous ne verrez pas directement le code Gleam, mais le code Erlang dans lequel il est compilé, ce qui est quand même assez utile.

## Plongée en profondeur
Gleam est un langage jeune, donc alors qu'il s'appuie sur les épaules de l'écosystème Erlang, les outils de débogage natifs de Gleam ne sont pas encore sous les projecteurs. Cela signifie que nous utilisons les outils éprouvés d'Erlang, et ce n'est pas une mauvaise chose. Le débogueur d'Erlang existe depuis les années 90, perfectionné au fil des ans pour éradiquer les bugs tenaces dans des systèmes où la fiabilité est clé.

Quant aux alternatives, le traçage est une méthode puissante dans le monde de BEAM (c'est la machine virtuelle qui exécute le code Erlang et Elixir). En utilisant `rebar3`, vous pouvez accéder à des outils comme `recon` pour tracer les appels de fonction et plonger profondément dans les problèmes de performance.

Passer de l'écriture en Gleam au débogage en Erlang peut sembler comme si vous traduisiez vos pensées à la volée. Mais l'avantage est que vous obtenez un aperçu du monde Erlang, comprenant les blocs de construction de votre application dans sa forme d'exécution.

## Voir Aussi
Pour élargir votre boîte à outils de débogage, consultez :

- La documentation du débogueur d'Erlang : [https://erlang.org/doc/apps/debugger/debugger_chapter.html](https://erlang.org/doc/apps/debugger/debugger_chapter.html)
- La bibliothèque `recon` pour Erlang : [https://ferd.github.io/recon/](https://ferd.github.io/recon/)
