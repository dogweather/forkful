---
title:                "Vérification de l'existence d'un répertoire"
html_title:           "Ruby: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous programmez en Ruby, il peut être utile de vérifier si un répertoire existe avant de l'utiliser dans votre code. Cela permet de s'assurer que le répertoire est présent et disponible, évitant ainsi les erreurs inattendues dans votre application.

## Comment faire

Pour vérifier si un répertoire existe en Ruby, vous pouvez utiliser la méthode `Dir.exist?()` en passant le chemin du répertoire en argument. Voici un exemple de code avec un répertoire existant :

```Ruby
if Dir.exist?("chemin/du/repetoire")
  puts "Le répertoire existe"
end
```

Et voici un exemple avec un répertoire qui n'existe pas :

```Ruby
if Dir.exist?("chemin/invalide")
  puts "Le répertoire existe"
end

# Output:
# Aucun message n'est affiché car le répertoire n'existe pas
```

Si vous souhaitez vérifier si un répertoire existe sans afficher de message, vous pouvez utiliser la méthode `Dir.exist?()` dans une condition :

```Ruby
if Dir.exist?("chemin/du/repetoire")
  # Le code à exécuter si le répertoire existe
else
  # Le code à exécuter si le répertoire n'existe pas
end
```

## Plongée en profondeur

En utilisant la méthode `Dir.exist?()`, Ruby va vérifier si le chemin fourni correspond à un répertoire existant. Si c'est le cas, la méthode renverra `true`, sinon elle renverra `false`. Notez que cette méthode ne vérifie pas si le chemin est un répertoire valide, elle se contente de vérifier s'il existe quelque chose à cet emplacement précis.

Il existe également la méthode `Dir.exists?()` qui a exactement le même fonctionnement que `Dir.exist?()` mais elle est dépréciée depuis la version 2.3 de Ruby. Il est donc recommandé d'utiliser la méthode `Dir.exist?()` pour vérifier si un répertoire existe.

## Voir aussi

- [Documentation sur la méthode Dir.exist?()](https://ruby-doc.org/core-#{RUBY_VERSION}/Dir.html#method-c-exists-3F)
- [Documentation sur la méthode Dir.exists?()](https://ruby-doc.org/core-#{RUBY_VERSION}/Dir.html#method-c-exists-3F)
- [Article sur la gestion des erreurs en Ruby](https://www.rubyguides.com/2019/02/ruby-error-handling/)