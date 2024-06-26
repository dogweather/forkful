---
date: 2024-01-26 00:50:48.669695-07:00
description: "Comment faire : En Elixir, nous utilisons souvent l'appariement de motifs\
  \ (pattern matching) et l'instruction `case` pour g\xE9rer diff\xE9rents r\xE9sultats,\
  \ y\u2026"
lastmod: '2024-03-13T22:44:57.335267-06:00'
model: gpt-4-1106-preview
summary: "En Elixir, nous utilisons souvent l'appariement de motifs (pattern matching)\
  \ et l'instruction `case` pour g\xE9rer diff\xE9rents r\xE9sultats, y compris les\
  \ erreurs."
title: Gestion des erreurs
weight: 16
---

## Comment faire :
En Elixir, nous utilisons souvent l'appariement de motifs (pattern matching) et l'instruction `case` pour gérer différents résultats, y compris les erreurs.

```elixir
defmodule Example do
  def divide(a, b) do
    case b do
      0 -> {:error, "Impossible de diviser par zéro."}
      _ -> {:ok, a / b}
    end
  end
end

# Division réussie
{:ok, result} = Example.divide(10, 2)
IO.puts("10 / 2 est #{result}")

# Tentative de division par zéro
{:error, reason} = Example.divide(10, 0)
IO.puts("Erreur : #{reason}")
```

Exemple de sortie :
```
10 / 2 est 5.0
Erreur : Impossible de diviser par zéro.
```

Lorsque vous exécutez ce code Elixir, vous obtenez soit une division réussie, soit un message d'erreur, en fonction de votre entrée. Pas de plantages ici !

## Plongée approfondie
Autrefois, la gestion des erreurs était souvent une question de vérification des valeurs retournées. Cependant, avec les racines fonctionnelles d'Elixir, nous avons l'appariement de motifs et les tuples étiquetés, comme `{:ok, value}` ou `{:error, reason}`, qui sont plus élégants.

Il existe d'autres manières de gérer les erreurs en Elixir :

- **L'`try` et `rescue` d'Elixir**, qui ressemblent au traditionnel `try-catch` des langages impératifs mais sont moins fréquemment utilisés en raison de la préférence d'Elixir pour l'explicité.
- **Les superviseurs et GenServers**, faisant partie du framework OTP d'Elixir, qui concernent davantage la tolérance aux fautes. Ils surveillent le processus de votre code, prêts à le redémarrer si les choses tournent mal.

Concernant l'implémentation, Elixir s'appuie sur la robustesse d'Erlang. Il traite les erreurs comme juste un autre type de message à gérer avec toute l'élégance de l'appariement de motifs et des fonctions.

## Voir aussi
Pour en savoir plus sur la gestion des erreurs en Elixir, consultez :

- Le guide officiel d'Elixir sur la [gestion des erreurs](https://elixir-lang.org/getting-started/try-catch-and-rescue.html).
- Apprenez-en davantage sur les [processus et OTP](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html).
- Le forum Elixir est toujours un bon endroit pour poser des questions : [https://elixirforum.com](https://elixirforum.com).
