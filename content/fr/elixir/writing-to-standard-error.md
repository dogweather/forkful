---
title:                "Écrire dans l'erreur standard"
html_title:           "Arduino: Écrire dans l'erreur standard"
simple_title:         "Écrire dans l'erreur standard"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Écrire sur la sortie d'erreur standard, c'est envoyer des messages d'erreur ou diagnostics distincts de la sortie principale d'un programme. Cela aide à séparer les erreurs du flux de données normal, facilitant le débogage et la logistique.

## How to (Comment Faire) :
```elixir
# Envoyer un simple message à stderr
:io.stderr(:hello_world)
# Résultat: :hello_world

# Écrire sur stderr avec IO.warn/1
IO.warn("Attention! Il y a une erreur.")
# Résultat: Attention! Il y a une erreur.
```

## Deep Dive (Plongée Profonde)
Historiquement, la distinction entre sortie standard et sortie d'erreur permet la redirection indépendante des logs d'erreur des résultats d'exécution dans les systèmes Unix. En Elixir, on utilise souvent `IO.warn/1` pour la sortie d'erreur simple. Pour plus de contrôle, on manipule directement le port de `:stderr` avec `:io.format/2`. 

## See Also (Voir Aussi)
- Documentation Elixir pour IO: https://hexdocs.pm/elixir/IO.html
- Guide d'introduction au Mix et OTP: https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html
- Unix Standard Streams: https://en.wikipedia.org/wiki/Standard_streams
