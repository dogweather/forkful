---
title:    "Ruby: Suppression de caractères correspondants à un modèle"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes peut-être en train de travailler sur un projet en Ruby et vous vous demandez pourquoi vous auriez besoin de supprimer des caractères correspondant à un modèle dans votre code. Eh bien, il y a plusieurs raisons possibles. Peut-être que vous voulez nettoyer ou reformater une chaîne de caractères avant de la passer à une autre fonction ou méthode. Ou peut-être que vous souhaitez simplement supprimer des caractères inutiles d'une chaîne pour faciliter la lecture et le traitement des données.

Quelle que soit la raison, supprimer des caractères en fonction d'un modèle peut être une tâche utile et courante en programmation Ruby. Voyons maintenant comment le faire.

## Comment faire

Pour supprimer des caractères en fonction d'un modèle dans Ruby, nous allons utiliser la méthode `.gsub()`. Cela nous permet de remplacer tous les caractères correspondant à un motif par une chaîne vide.

```Ruby
phrase = "Je suis un blogueur passionné de Ruby"
nouvelle_phrase = phrase.gsub(/[aeiou]/, '')

puts nouvelle_phrase
# J s sm blgr pssnné d Rby
```

Dans cet exemple, nous utilisons une expression régulière comme modèle pour la méthode `.gsub()`. La regex `[aeiou]` signifie que nous voulons remplacer toutes les voyelles par une chaîne vide. Vous pouvez bien sûr utiliser n'importe quel modèle ou caractères que vous souhaitez supprimer de votre chaîne.

## Plongée en profondeur

Maintenant que nous avons vu comment supprimer des caractères en fonction d'un modèle, il est important de noter que la méthode `.gsub()` peut également prendre un bloc en tant que deuxième argument.

```Ruby
phrase = "Je suis un blogueur passionné de Ruby"

nouvelle_phrase = phrase.gsub(/[aeiou]/) { |match| match.upcase }
puts nouvelle_phrase
# J S SBlOgr Pssnné D RBy
```

Dans cet exemple, nous utilisons le bloc pour transformer chaque caractère correspondant en sa version en majuscule, plutôt que de simplement le supprimer.

Il est également possible d'utiliser la méthode `.delete()` pour supprimer des caractères en fonction d'un modèle, mais cela n'est possible que si vous souhaitez supprimer des caractères individuels plutôt que des motifs plus complexes.

Enfin, il est important de noter que la méthode `.gsub()` peut être utilisée pour remplacer les caractères par n'importe quelle chaîne de caractères, pas seulement une chaîne vide. Vous pouvez également utiliser des expressions régulières plus avancées pour des schémas de remplacement plus complexes.

## Voir aussi

- Documentation sur la méthode `.gsub()`: https://ruby-doc.org/core-2.7.2/String.html#method-i-gsub
- Tutoriel sur les expressions régulières en Ruby: https://www.rubyguides.com/2015/06/ruby-regex/
- Article sur la méthode `.delete()`: https://www.rubyguides.com/2019/05/ruby-string-delete/