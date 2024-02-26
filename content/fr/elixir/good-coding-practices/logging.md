---
date: 2024-01-26 01:02:35.248928-07:00
description: "La journalisation dans le d\xE9veloppement de logiciels est la technique\
  \ permettant d'enregistrer les \xE9v\xE9nements qui se produisent pendant l'ex\xE9\
  cution d'un\u2026"
lastmod: '2024-02-25T18:49:54.217265-07:00'
model: gpt-4-1106-preview
summary: "La journalisation dans le d\xE9veloppement de logiciels est la technique\
  \ permettant d'enregistrer les \xE9v\xE9nements qui se produisent pendant l'ex\xE9\
  cution d'un\u2026"
title: Journalisation
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
La journalisation dans le développement de logiciels est la technique permettant d'enregistrer les événements qui se produisent pendant l'exécution d'un programme, typiquement dans un fichier ou un système externe. Les programmeurs le font pour obtenir des aperçus du comportement du logiciel, dépanner des problèmes, et maintenir un enregistrement de l'historique opérationnel qui est crucial pour le débogage et la surveillance de la santé des applications.

## Comment faire :
Dans Elixir, la méthode principale pour enregistrer des informations est l'utilisation du module intégré `Logger`. Voici comment vous pouvez l'utiliser :

```elixir
defmodule MyApplication do
  require Logger

  def do_something_important(param) do
    Logger.info("Démarrage d'un processus important avec le paramètre : #{param}")

    # Simulation d'un travail en cours
    :timer.sleep(1000)

    Logger.debug("Processus terminé.")
  rescue
    error -> Logger.error("Une erreur s'est produite : #{inspect(error)}")
  end
end

# Pour voir vos journaux, vous appelez juste la fonction :
MyApplication.do_something_important("MonParam")
```

Ce simple extrait montre comment enregistrer différents niveaux (`info`, `debug` et `error`). Lorsque vous exécutez ceci, vous ne verrez pas le message de débogage à moins de configurer le niveau de Logger sur `:debug`. Par défaut, le Logger d'Elixir filtre les messages de journalisation en dessous du niveau `:info`.

Un exemple de sortie au niveau `:info` pourrait ressembler à ceci :
```
14:32:40.123 [info]  Démarrage d'un processus important avec le paramètre : MonParam
14:32:41.126 [error] Une erreur s'est produite : %RuntimeError{message: "erreur d'exécution"}
```

## Approfondissement :
Le `Logger` d'Elixir est un outil intégré qui fait partie du langage depuis ses tout débuts. Il est influencé par les systèmes de journalisation d'autres langages BEAM comme Erlang. Le logger fournit différents niveaux de journalisation – `:debug`, `:info`, `:warn` et `:error` – et il est modulable, permettant de raccorder différents backends pour gérer les messages de journalisation.

Une alternative au Logger intégré pour des scénarios plus complexes est l'utilisation de bibliothèques de journalisation telles que `Logstash` ou `Sentry` pour Elixir, qui peuvent fournir des fonctionnalités supplémentaires comme le suivi des erreurs et l'agrégation dans un format plus visuel. Pour le développement local, les développeurs Elixir s'appuient souvent sur les fonctionnalités intégrées de Logger pour sa simplicité et son intégration avec la machine virtuelle BEAM.

Sous le capot, le module Logger propose une journalisation asynchrone et synchrone. La journalisation asynchrone, qui est le réglage par défaut, ne bloque pas l'exécution de votre application pendant l'enregistrement des messages. Cela garantit que la journalisation n'affecte pas négativement les performances. Cependant, la journalisation synchrone peut être activée pour les cas où vous devez garantir que les messages sont enregistrés dans l'ordre où ils ont été envoyés.

La configuration du Logger peut être ajustée dans le fichier `config/config.exs` d'une application Elixir, où vous pouvez définir le niveau de journalisation, le format, les métadonnées, et plus encore. Souvenez-vous toujours d'ajuster vos niveaux de journalisation et de sorties pour différents environnements ; vous ne voudriez pas que des journaux de débogage verbeux inondent vos systèmes de production.

## Voir aussi :
- La documentation officielle de Logger Elixir : https://hexdocs.pm/logger/Logger.html
- Un article de blog sur les meilleures pratiques de journalisation Elixir : https://blog.appsignal.com/2020/05/06/elixir-logging-tips-and-tricks.html
- Sentry pour Elixir sur Hex : https://hex.pm/packages/sentry
- La leçon de l'Elixir School sur Logger : https://elixirschool.com/en/lessons/specifics/debugging/#logging
