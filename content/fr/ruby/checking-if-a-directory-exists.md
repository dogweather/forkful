---
title:    "Ruby: Vérification de l'existence d'un répertoire"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

La vérification de l'existence d'un répertoire est une tâche courante dans la programmation Ruby. Cette étape est importante car elle permet de s'assurer qu'un chemin de fichier est valide avant d'effectuer des opérations dessus. Dans cet article, nous allons explorer comment vérifier si un répertoire existe dans Ruby et pourquoi c'est crucial pour votre code.

## Comment faire

Pour vérifier si un répertoire existe dans Ruby, nous pouvons utiliser la méthode `Dir.exist?` en lui passant le chemin du répertoire en paramètre. Voici un exemple de code pour illustrer cela :

```ruby
if Dir.exist?("/chemin/dossier")
    puts "Le dossier existe."
else
    puts "Le dossier n'existe pas."
end
```

Dans cet exemple, nous utilisons la méthode `Dir.exist?` pour vérifier si le dossier "/chemin/dossier" existe. Si c'est le cas, nous affichons un message indiquant que le dossier existe, sinon nous affichons un message indiquant qu'il n'existe pas. Voici ce que vous pouvez attendre comme résultat :

```ruby
Le dossier existe.
```

## Plongée en profondeur

Pour mieux comprendre comment fonctionne la méthode `Dir.exist?` et pourquoi c'est si important, nous allons explorer quelques détails techniques.

Tout d'abord, cette méthode est une méthode de classe de la classe `Dir`. Cela signifie que nous pouvons l'appeler directement sans avoir besoin d'instancier un objet `Dir`. De plus, cette méthode renvoie une valeur booléenne, `true` si le dossier existe et `false` sinon.

En interne, la méthode `Dir.exist?` utilise la fonction système `File.exist?` pour effectuer la vérification. Cela signifie qu'elle peut également renvoyer `true` si un fichier portant le même nom que le dossier existe. C'est pourquoi il est important de s'assurer qu'il s'agit bien d'un dossier avant d'effectuer des opérations dessus.

Enfin, il est également possible d'utiliser la méthode `Dir.exists?` qui est un alias de `Dir.exist?`, vous pouvez donc utiliser celle que vous préférez dans votre code.

## Voir aussi

Voici quelques liens utiles pour en savoir plus sur la vérification de l'existence d'un répertoire en Ruby :

- Documentation officielle de Ruby sur la méthode `Dir.exist?` : https://ruby-doc.org/core/Dir.html#method-c-exist-3F
- Documentation officielle de Ruby sur la méthode `File.exist?` : https://ruby-doc.org/core/File.html#method-c-exist-3F
- Un tutoriel pratique sur la vérification de l'existence d'un répertoire en Ruby : https://www.rubyguides.com/2016/09/check-if-file-exists/

En utilisant la méthode `Dir.exist?` dans votre code, vous pourrez facilement vérifier si un répertoire existe et ainsi éviter des erreurs et des bogues potentiellement graves. N'hésitez pas à explorer d'autres options et méthodes pour manipuler les fichiers et les dossiers en Ruby !