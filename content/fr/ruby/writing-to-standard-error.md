---
title:                "Ruby: Écrire vers le flux d'erreur standard"
simple_title:         "Écrire vers le flux d'erreur standard"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi 

Écrire sur la sortie d'erreur standard en Ruby est une pratique courante pour les développeurs. Cela permet de mieux comprendre les erreurs et les bugs dans le code, ainsi que de les résoudre plus efficacement. Cela peut également aider à déboguer des problèmes dans les applications en production.

## Comment Faire 

Pour écrire sur la sortie d'erreur standard en Ruby, il suffit d'utiliser la méthode `STDERR.puts` suivi du message que l'on souhaite afficher. Voici un exemple de code :

```Ruby
STDERR.puts "Une erreur s'est produite"
```

Cela affichera le message "Une erreur s'est produite" dans la sortie d'erreur standard. Voici un autre exemple où nous utilisons la méthode `STDERR.write` :

```Ruby
STDERR.write "Un autre exemple d'erreur"
```

Cela affichera le message "Un autre exemple d'erreur" sans sauter de ligne. Vous pouvez également utiliser `STDERR.print` si vous souhaitez un comportement similaire à celui de `puts`.

Il est également possible d'écrire sur la sortie d'erreur standard en utilisant la syntaxe `warn` :

```Ruby
warn "Attention ! Une erreur s'est produite"
```

## Plongée En Profondeur 

La sortie d'erreur standard en Ruby est utile pour plusieurs raisons. Tout d'abord, elle permet de différencier les erreurs des autres messages affichés dans la sortie standard. Cela peut être particulièrement utile lors du débogage, car cela nous permet de voir immédiatement qu'il y a eu une erreur.

En outre, écrire sur la sortie d'erreur standard est également utile pour les applications en production. Cela permet de suivre facilement les erreurs qui se produisent dans l'application et de les enregistrer dans un fichier de journal pour une analyse ultérieure.

Il est également possible de rediriger la sortie d'erreur standard vers un autre flux, comme une variable ou un fichier, en utilisant les méthodes `STDERR.reopen` ou `warn`. Cela peut être utile pour le débogage ou pour le traitement des erreurs dans différentes parties de l'application.

Enfin, il est important de noter que la sortie d'erreur standard et la sortie standard peuvent être combinées en utilisant `STDOUT` et `STDERR` ensemble. Cela peut être utile pour afficher des messages de débogage à la fois dans la sortie standard et la sortie d'erreur standard.

## Voir Aussi 

- [Documentation officielle de Ruby sur la sortie d'erreur standard](https://ruby-doc.org/core-2.7.0/IO.html#method-i-puts)
- [Article sur la redirection des flux de sortie en Ruby](https://www.rubyguides.com/2019/03/ruby-stdin-stdout-stderr/)
- [Vidéo sur l'utilisation de la sortie d'erreur standard en Ruby](https://www.youtube.com/watch?v=l8hfLB2aMCo)