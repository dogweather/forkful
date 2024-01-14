---
title:    "Ruby: Écrire sur la sortie standard"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Pourquoi

Ecrire vers l'erreur standard (standard error en anglais) est une technique utile en programmation pour afficher des messages d'erreur ou de débogage pendant l'exécution d'un programme. Cela permet de comprendre et de résoudre plus facilement les erreurs qui peuvent survenir pendant le développement.

## Comment faire

Écrire vers l'erreur standard est très simple en Ruby. Il suffit d'utiliser la méthode puts en y passant en paramètre le message que l'on souhaite afficher. Par exemple :

```Ruby
puts "Il y a eu une erreur dans l'exécution du programme"
```

Cela affichera le message "Il y a eu une erreur dans l'exécution du programme" dans la console, qui correspond à l'erreur standard.

## Plongée en profondeur

En plus de simplement afficher des messages d'erreur, écrire vers l'erreur standard permet également de différencier les erreurs provenant du système d'exploitation et celles générées par le programme lui-même. Les erreurs provenant du système d'exploitation sont généralement écrites vers la sortie standard (standard output), donc en écrivant vers l'erreur standard, on peut facilement les distinguer.

Il est également possible d'écrire des messages de débogage vers l'erreur standard en utilisant la méthode STDERR.puts. Ainsi, lors de l'exécution d'un programme, les messages d'erreur et les messages de débogage seront séparés, ce qui facilitera leur compréhension et leur gestion.

## Voir aussi

- [Documentation officielle de Ruby sur l'écriture vers l'erreur standard](https://ruby-doc.org/core-2.7.1/IO.html#method-i-puts)
- [Article sur les différences entre l'erreur standard et la sortie standard en programmation](https://blog.ombulabs.com/stdout-vs-stderr.html)
- [Guide sur le débogage en Ruby](https://www.rubyguides.com/2020/01/ruby-debugging/)