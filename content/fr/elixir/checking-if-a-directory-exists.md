---
title:    "Elixir: Vérification de l'existence d'un répertoire"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Pourquoi

Il est important de savoir si un répertoire existe lorsque vous travaillez avec des fichiers et des données dans Elixir. Cela peut vous aider à garantir que votre code fonctionne correctement et à éviter les erreurs.

## Comment faire

Voici un exemple de code qui vérifie si un répertoire existe en utilisant la fonction `File.dir?` :

```elixir
if File.dir?("chemin/vers/mon/répertoire") do
  IO.puts "Le répertoire existe."
else
  IO.puts "Le répertoire n'existe pas."
end
```

Si le répertoire existe, vous verrez la sortie `Le répertoire existe.` sinon vous verrez `Le répertoire n'existe pas.`

## Plongée profonde

La fonction `File.dir?` vérifie si un chemin donné correspond à un répertoire existant. Elle renvoie `true` si c'est le cas et `false` sinon. Si le chemin indiqué pointe vers un lien symbolique vers un répertoire existant, la fonction le considérera comme un répertoire existant.

Il est important de noter que la fonction `File.dir?` ne créera pas le répertoire si celui-ci n'existe pas. Si vous souhaitez créer un répertoire s'il n'existe pas, vous pouvez utiliser la fonction `File.mkdir`.

## Voir aussi

- Documentation sur la fonction `File.dir?` : https://hexdocs.pm/elixir/File.html#dir?/1
- Documentation sur la fonction `File.mkdir` : https://hexdocs.pm/elixir/File.html#mkdir/1