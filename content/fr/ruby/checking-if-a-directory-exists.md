---
title:                "Ruby: Vérifier l'existence d'un répertoire"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Pourquoi

Vérifier si un répertoire existe est une tâche courante en programmation, notamment en Ruby. Cela permet de s'assurer que le code s'exécutera correctement et de gérer les erreurs potentielles.

# Comment faire

Pour vérifier si un répertoire existe en Ruby, il suffit d'utiliser la méthode `File.exist?("chemin/vers/le/répertoire")`. Cette méthode renvoie un booléen, `true` si le répertoire existe et `false` s'il n'existe pas.

```Ruby
if File.exist?("/home/user/Documents")
  puts "Le répertoire existe"
else
  puts "Le répertoire n'existe pas"
end

# Output:
# Le répertoire existe
```

On peut également utiliser la méthode `File.directory?` qui renvoie `true` si le chemin donné correspond à un répertoire ou `false` sinon.

```Ruby
if File.directory?("/home/user/Documents")
  puts "C'est un répertoire"
else
  puts "Ce n'est pas un répertoire"
end

# Output:
# C'est un répertoire
```

# Plongeon en profondeur

Il est important de noter que ces méthodes ne vérifient que si le chemin donné correspond à un répertoire physique sur le système de fichiers. Elles ne vérifient pas si le répertoire est accessible ou si l'utilisateur a les permissions nécessaires pour y accéder.

Pour vérifier ces éléments, on peut utiliser la méthode `File.readable?` pour vérifier si le répertoire est accessible en lecture, `File.writable?` pour vérifier s'il est accessible en écriture et `File.executable?` pour vérifier s'il est exécutable.

```Ruby
if File.readable?("/home/user/Documents")
  puts "Le répertoire est accessible en lecture"
end
if File.writable?("/home/user/Documents")
  puts "Le répertoire est accessible en écriture"
end
if File.executable?("/home/user/Documents")
  puts "Le répertoire est exécutable"
end

# Output:
# Le répertoire est accessible en lecture
```

# Voir aussi

- [Ruby documentation officielle sur la classe File](https://ruby-doc.org/core-3.0.0/File.html)
- [Tutoriel sur les opérations de base de fichiers en Ruby](https://codecademy.com/learn/learn-ruby/modules/learn-ruby-files/cheatsheet)
- [Utiliser la gem `fileutils` pour manipuler des fichiers](https://dev.to/itsmohsen/how-to-manipulate-files-in-ruby-1d94)