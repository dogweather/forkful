---
title:                "Vérifier si un répertoire existe"
html_title:           "Elixir: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

**Qu'est-ce que c'est et pourquoi?**

Vérifier si un répertoire existe est simplement le fait de voir si un dossier spécifique se trouve dans un emplacement donné sur votre ordinateur. Les programmeurs le font pour s'assurer que certaines ressources nécessaires à leur code sont disponibles avant de les utiliser. Cela permet d'éviter les erreurs et d'optimiser les performances.

**Comment faire :**

Pour vérifier si un répertoire existe en utilisant Elixir, vous pouvez utiliser la fonction `File.exists?/1` en lui passant le chemin du répertoire en tant que paramètre. Si le répertoire existe, la fonction renverra `true`, sinon elle renverra `false`. Voici un exemple de code :

```
Elixir

if File.exists?("chemin/vers/le/répertoire")
  IO.puts("Le répertoire existe !")
else
  IO.puts("Le répertoire n'existe pas.")
end

```

**Un peu plus en détails :**

Auparavant, la vérification de l'existence d'un répertoire nécessitait plusieurs étapes telles que la recherche manuelle du répertoire ou l'utilisation de commandes système externes. Grâce à Elixir, cette tâche est désormais plus simple et intégrée dans le langage lui-même.

Bien sûr, il existe d'autres moyens de vérifier l'existence d'un répertoire tels que l'utilisation de bibliothèques externes comme [ex_file](https://hex.pm/packages/ex_file) ou [fsex](https://hex.pm/packages/fsex). Mais la fonction `File.exists?/1` est la méthode la plus simple et la plus efficace si vous utilisez déjà Elixir.

Il est également intéressant de noter que `File.exists?/1` peut également être utilisée pour vérifier l'existence de fichiers ou d'autres types de ressources.

**A voir aussi :**

- Documentation officielle d'Elixir pour [File.exists?/1](https://hexdocs.pm/elixir/File.html#exists?/1)
- Package [ex_file](https://hex.pm/packages/ex_file)
- Package [fsex](https://hex.pm/packages/fsex)