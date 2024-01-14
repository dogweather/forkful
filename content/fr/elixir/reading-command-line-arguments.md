---
title:    "Elixir: La lecture des arguments en ligne de commande"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes nouveau dans le monde de la programmation en Elixir, vous pourriez vous demander pourquoi il est important de lire les arguments de ligne de commande. Tout d'abord, il est essentiel de comprendre comment votre programme communique avec l'utilisateur via les arguments de ligne de commande. Cela vous permettra de créer des applications plus interactives et personnalisées.

## Comment faire

Parlons maintenant de la façon de lire les arguments de ligne de commande en Elixir. Tout d'abord, nous devons utiliser le module `OptionParser` qui nous permet de spécifier les options attendues pour notre programme. Voici un exemple de code qui lit deux arguments de ligne de commande, un pour le nom et un pour l'âge :

```
Elixir
defmodule CommandLine do
  def parse_args(args) do
    OptionParser.parse(args, switches: [name: :string, age: :integer])
  end
end

args = ["--name", "Pierre", "--age", "25"]
result = CommandLine.parse_args(args)

IO.puts "Bonjour #{result[:name]}, tu as #{result[:age]} ans !"
```

En entrant ces lignes de commande dans le terminal, vous devriez obtenir le résultat suivant :

```
Bonjour Pierre, tu as 25 ans !
```

Comme vous pouvez le voir, nous avons utilisé les options `string` et `integer` pour spécifier le type de données attendu pour chaque argument. Vous pouvez également utiliser d'autres types tels que `boolean`, `float`, etc. pour définir vos options.

## Plongée en profondeur

Bien que le module `OptionParser` soit simple et pratique pour lire les arguments de ligne de commande, il y a d'autres façons d'y accéder. Par exemple, vous pouvez utiliser l'argument spécial `$*ARGS` qui renvoie une liste des arguments entrés par l'utilisateur. En utilisant cette méthode, vous n'êtes pas limité aux options spécifiées à l'avance et pouvez modifier votre code pour lire n'importe quel argument donné.

## Voir aussi

- [Documentation officielle d'Elixir sur la lecture des arguments de ligne de commande](https://hexdocs.pm/elixir/OptionParser.html)
- [Tutoriel vidéo sur la lecture des arguments de ligne de commande en Elixir](https://www.youtube.com/watch?v=_qDAvXsk5bU)
- [Article sur la gestion des erreurs avec les arguments de ligne de commande en Elixir](https://medium.com/@jamesaenera/elixir-tutorial-command-line-arguments-and-error-handling-c1244bd38497)