---
title:                "Ruby: Ecrire vers l'erreur standard"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Pourquoi

L'écriture sur l'erreur standard est une technique utile pour déboguer et comprendre les erreurs dans le code Ruby. Cela permet aux développeurs de mieux comprendre où le code est en train d'échouer et comment le résoudre.

# Comment faire

Pour écrire sur l'erreur standard, utilisez la méthode `STDERR.puts` suivi du message que vous souhaitez afficher. Par exemple :

```Ruby
STDERR.puts "Ce message est écrit sur l'erreur standard."
```

Cela affichera le message sur la sortie d'erreur, plutôt que sur la sortie standard.

Il est également possible de rediriger la sortie d'erreur vers un fichier en utilisant l'opérateur de redirection `>` dans la ligne de commande. Par exemple :

```Ruby
ruby script.rb > output.txt
```

Cela redirigera toute la sortie du script vers le fichier output.txt, y compris les messages d'erreur.

# Plongée en profondeur

Alors que la sortie standard est destinée à afficher les résultats de votre code, la sortie d'erreur est utilisée pour les messages d'erreurs et les informations de débogage. Cela signifie que vous pouvez utiliser l'écriture sur l'erreur standard pour fournir des informations supplémentaires sur les erreurs qui se produisent dans votre code.

De plus, en utilisant la méthode `STDERR.puts`, vous pouvez formatter les messages d'erreur avec des couleurs et des styles pour les rendre plus visibles et faciles à lire. Cela peut être particulièrement utile lorsque vous analysez de grands volumes d'erreurs.

# Voir également

- [Documentation de Ruby sur la sortie d'erreur](https://ruby-doc.org/core-2.7.1/IO.html#method-c-new-label-Error+Output)
- [Article sur la redirection de la sortie d'erreur en Ruby](https://www.rubyguides.com/2018/10/standard-error-output-in-ruby/)
- [Vidéo sur l'utilisation de l'écriture sur l'erreur standard en Ruby](https://www.youtube.com/watch?v=s6kT5Mik_rQ)