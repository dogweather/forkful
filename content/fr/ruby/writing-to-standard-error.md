---
title:    "Ruby: Écriture vers le flux d'erreur standard"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

L'écriture vers la sortie d'erreur (standard error) est une compétence utile à maîtriser en programmation Ruby. Elle vous permet de mieux comprendre les erreurs et les bogues dans votre code, ce qui rend le débogage plus facile et efficace.

## Comment faire

Pour écrire vers la sortie d'erreur, vous pouvez utiliser la méthode `puts` avec le mot-clé `STDERR`. Voici un exemple de code :

```ruby
$stderr.puts "Une erreur s'est produite!"
```

Cela écrira le message "Une erreur s'est produite!" dans la sortie d'erreur, plutôt que la sortie standard. Vous pouvez également utiliser `STDERR.print` pour imprimer un message sans saut de ligne à la fin.

La sortie d'erreur peut également être utilisée pour afficher des messages de débogage. Voici un autre exemple de code qui affiche le contenu d'une variable dans la sortie d'erreur :

```ruby
a = [1, 2, 3]
$stderr.puts "Contenu de a : #{a}"
```

Lorsque vous exécutez ce code, vous verrez le contenu de la variable `a` dans la sortie d'erreur, ce qui peut être utile pour trouver l'origine d'un problème dans votre code.

Les captures d'erreurs `(rescue)` sont également un bon endroit pour écrire vers la sortie d'erreur. Cela peut vous aider à déterminer quelle erreur a été levée et à quel moment.

## Plongée en profondeur

L'écriture vers la sortie d'erreur peut être particulièrement utile pour déboguer du code en utilisant des outils comme `irb` ou `pry`. Ces outils affichent les erreurs dans la sortie d'erreur et les séparent clairement de la sortie standard, facilitant ainsi la compréhension des erreurs.

Il est également important de noter que la plupart des systèmes de logiciels utilisent la sortie d'erreur pour enregistrer les erreurs et les avertissements. En écrivant vers la sortie d'erreur, vous facilitez la collecte et l'analyse de ces erreurs dans vos applications.

Enfin, l'écriture vers la sortie d'erreur peut également être utilisée pour afficher des messages d'état ou de progression lors de l'exécution de votre code, en informant les utilisateurs qu'une certaine tâche a été effectuée ou qu'une erreur est survenue.

## Voir aussi

- [Documentation officielle de Ruby sur STDERR](https://ruby-doc.org/core-2.7.1/IO.html#method-c-new-label-IO+and+File+open+mode)
- [Tutorial sur les sorties en Ruby](https://www.rubyguides.com/2018/10/puts-vs-print-vs-p-vs-printf/)

Merci d'avoir lu cet article. Utilisez l'écriture vers la sortie d'erreur dans votre code Ruby pour améliorer votre expérience de débogage et faciliter la gestion des erreurs. À bientôt pour un prochain article !