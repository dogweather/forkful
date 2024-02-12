---
title:                "Écrire sur l'erreur standard"
aliases:
- /fr/elixir/writing-to-standard-error.md
date:                  2024-02-03T19:32:51.573339-07:00
model:                 gpt-4-0125-preview
simple_title:         "Écrire sur l'erreur standard"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et pourquoi ?

Écrire sur l'erreur standard (stderr) en Elixir est une méthode permettant de diriger les messages d'erreur et les diagnostics à part de la sortie principale (stdout). Les programmeurs utilisent stderr pour déboguer et gérer les erreurs sans encombrer la sortie principale du programme, ce qui facilite l'identification et la résolution des problèmes.

## Comment faire :

En Elixir, vous pouvez utiliser des fonctions du module `IO` telles que `IO.puts/2` et `IO.warn/2` pour écrire des messages sur l'erreur standard :

```elixir
# Écrire un message simple sur stderr
IO.puts(:stderr, "Erreur : Quelque chose s'est mal passé !")

# Utiliser IO.warn, qui est plus sémantique pour les avertissements/erreurs
IO.warn("Avertissement : Vous êtes sur le point de dépasser la limite !")
```

Exemple de sortie dans le terminal pour `IO.puts/2`:
```
Erreur : Quelque chose s'est mal passé !
```

Pour `IO.warn/2`, la sortie serait similaire, mais `IO.warn/2` est spécifiquement conçu pour les avertissements et pourrait inclure un formatage ou un comportement supplémentaire dans les futures versions d'Elixir.

**Utiliser des bibliothèques tierces**

Bien que la bibliothèque standard d'Elixir soit généralement suffisante pour gérer la sortie d'erreur standard, vous pourriez trouver utiles des bibliothèques comme `Logger` pour des applications plus complexes ou pour configurer différents niveaux de logs et sorties.

Exemple d'utilisation de `Logger` pour afficher un message d'erreur :

```elixir
require Logger

# Configurer Logger pour sortir sur stderr
Logger.configure_backend(:console, device: :stderr)

# Écrire un message d'erreur
Logger.error("Erreur : Impossible de se connecter à la base de données.")
```

Cette configuration dirige spécifiquement la sortie du `Logger` vers stderr, ce qui est utile pour séparer la journalisation des erreurs des messages de journalisation standard.
