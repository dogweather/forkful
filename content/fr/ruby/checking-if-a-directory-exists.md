---
title:    "Ruby: Vérifier si un répertoire existe"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

# Pourquoi

Avant de commencer à apprendre comment vérifier si un répertoire existe en Ruby, il est important de comprendre pourquoi cette action peut être utile. La vérification de l'existence d'un répertoire peut être nécessaire lors de l'écriture de scripts ou de programmes qui doivent manipuler des fichiers et des dossiers. Cela permet de s'assurer que le code fonctionne correctement et évite tout problème d'exécution inattendu.

# Comment Faire

Pour vérifier si un répertoire existe en Ruby, nous pouvons utiliser la méthode `Dir.exist?` qui renvoie un booléen pour indiquer si le répertoire spécifié existe ou non. Par exemple, si nous voulons vérifier si le répertoire "documents" existe dans le dossier courant, nous pouvons utiliser le code suivant :

```Ruby
if Dir.exist?("documents")
  puts "Le répertoire existe."
else
  puts "Le répertoire n'existe pas."
end
```

Si le répertoire "documents" existe, la console affichera "Le répertoire existe." Sinon, elle affichera "Le répertoire n'existe pas."

# Plongée Profonde

La méthode `Dir.exist?` utilise le système de fichiers pour vérifier l'existence d'un répertoire. Cela signifie que si le répertoire spécifié est inaccessible pour une raison quelconque, la méthode renverra `false` même s'il existe en réalité. Il est également intéressant de noter que cette méthode ne vérifie que l'existence du répertoire spécifié, elle ne tient pas compte des permissions d'accès.

Il existe d'autres méthodes en Ruby telles que `File.exist?` et `Pathname.exist?` qui peuvent également être utilisées pour vérifier l'existence d'un répertoire. Cependant, selon le type de demande, l'utilisation de `Dir.exist?` est généralement la plus appropriée.

# Voir Aussi

- [La documentation officielle de Dir.exist?](https://ruby-doc.org/core-2.7/Dir.html#method-c-exist-3F)
- [Vérifier si un fichier existe en Ruby](https://www.sweetycode.com/blog/checking-if-file-exists-ruby/)
- [Lire et écrire dans un répertoire en Ruby](https://www.lewagon.com/fr/blog/ecrire-dans-un-repertoire-en-ruby)