---
aliases:
- /fr/ruby/handling-errors/
date: 2024-01-26 00:56:54.478607-07:00
description: "La gestion des erreurs consiste \xE0 anticiper l'inattendu dans le code\
  \ \u2014 \xE0 g\xE9rer les erreurs et les probl\xE8mes avec \xE9l\xE9gance sans\
  \ faire planter le programme.\u2026"
lastmod: 2024-02-18 23:09:09.415365
model: gpt-4-1106-preview
summary: "La gestion des erreurs consiste \xE0 anticiper l'inattendu dans le code\
  \ \u2014 \xE0 g\xE9rer les erreurs et les probl\xE8mes avec \xE9l\xE9gance sans\
  \ faire planter le programme.\u2026"
title: Gestion des erreurs
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

La gestion des erreurs consiste à anticiper l'inattendu dans le code — à gérer les erreurs et les problèmes avec élégance sans faire planter le programme. Les programmeurs le font pour contrôler le flux lorsque les choses tournent mal et pour maintenir une expérience utilisateur fluide.

## Comment faire :

Ruby utilise `begin`, `rescue`, `ensure`, et `end` pour gérer les erreurs. Vous encadrez le code à risque entre `begin` et `end`. Si une erreur se produit, `rescue` intervient.

```Ruby
begin
  # Le code à risque se trouve ici.
  puts 10 / 0
rescue ZeroDivisionError => e
  puts "Oups ! Vous ne pouvez pas faire ça : #{e.message}"
ensure
  puts "Ceci s'exécute toujours, qu'il y ait erreur ou non."
end
```

Exemple de sortie :
```
Oups ! Vous ne pouvez pas faire ça : divisé par 0
Ceci s'exécute toujours, qu'il y ait erreur ou non.
```

## Approfondissement

Historiquement, la gestion des erreurs dans les langages de programmation a beaucoup évolué, avec des anciens langages ayant souvent des mécanismes rudimentaires ou inexistants. La gestion des exceptions de Ruby est inspirée de langages comme Python et Smalltalk.

Les alternatives à `begin-rescue` dans Ruby incluent l'utilisation de `rescue` dans les définitions de méthodes ou l'emploi de `throw` et `catch` pour un contrôle de flux non standard, bien qu'ils ne soient pas utilisés pour la gestion typique des erreurs.

Un détail intéressant : les exceptions de Ruby sont des objets (instances de la classe `Exception` et de ses descendants), donc vous pouvez définir des classes d'erreurs personnalisées et faire plus que simplement enregistrer les erreurs — vous pouvez transporter un état riche à travers le programme pour une gestion des erreurs plus robuste.

## Voir Aussi

- La documentation de Ruby sur les exceptions et la gestion des erreurs : [ruby-doc.org](https://ruby-doc.org/core-3.1.0/doc/syntax/exceptions_rdoc.html)
- Un guide détaillé sur les meilleures pratiques de gestion des erreurs en Ruby : [thoughtbot.com](https://thoughtbot.com/blog/rescue-standarderror-not-exception)
