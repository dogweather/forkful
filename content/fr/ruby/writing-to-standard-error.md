---
title:                "Écrire sur la sortie d'erreur standard"
html_title:           "Ruby: Écrire sur la sortie d'erreur standard"
simple_title:         "Écrire sur la sortie d'erreur standard"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes peut-être en train de vous demander pourquoi vous devriez vous intéresser à écrire dans la sortie standard d'erreur en Ruby. Eh bien, tout d'abord, cela peut être utile pour déboguer votre code en affichant des messages d'erreur spécifiques en cas de problème. De plus, cela peut également être utile pour enregistrer des informations et des avertissements dans les fichiers de logs de votre application.

## Comment faire

Pour écrire dans la sortie standard d'erreur en Ruby, vous pouvez utiliser la méthode `warn` qui est prévue à cet effet. Voici un exemple de code avec une utilisation de `warn` et le résultat qui sera affiché dans votre terminal :

```Ruby
warn "Attention, une erreur s'est produite !"
```

```
Attention, une erreur s'est produite !
```

Comme vous pouvez le voir, le message est affiché en jaune et préfixé par le mot `warn` pour indiquer qu'il provient de la sortie standard d'erreur.

## Plongée en profondeur

Maintenant que vous savez comment utiliser la méthode `warn` pour écrire dans la sortie standard d'erreur, voyons quelques notions plus avancées. Tout d'abord, sachez que vous pouvez personnaliser la couleur du message en utilisant la gem `colorize`. Voici un exemple de code avec l'utilisation de `colorize` pour afficher un message d'erreur en rouge :

```Ruby
require 'colorize'
warn "Ce message d'erreur est en rouge !".colorize(:red)
```

```
This message d'erreur est en rouge !
```

Vous pouvez également utiliser la méthode `puts` pour écrire dans la sortie standard d'erreur, cependant, il est recommandé d'utiliser `warn` car cela est plus cohérent avec le but de l'écriture dans la sortie standard d'erreur.

Finalement, n'oubliez pas de toujours utiliser des messages clairs et concis et de ne pas surcharger la sortie standard d'erreur avec trop d'informations. Gardez à l'esprit que ces messages peuvent être utiles pour déboguer votre code mais ils peuvent également être vus par vos utilisateurs et doivent donc être compréhensibles pour eux.

## Voir aussi

- [Méthode `warn` dans la documentation officielle de Ruby](https://ruby-doc.org/core-3.0.2/Kernel.html#method-i-warn)
- [Gem `colorize` pour personnaliser les couleurs des messages d'erreur](https://rubygems.org/gems/colorize)