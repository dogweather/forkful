---
title:                "Rédaction de tests"
html_title:           "Arduino: Rédaction de tests"
simple_title:         "Rédaction de tests"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi & Pourquoi ?)
Écrire des tests, c'est comme préparer des mini-challenges pour son code: on vérifie que tout fonctionne. Les devs font ça pour dormir sur leurs deux oreilles, éviter les bugs, et simplifier les mises à jour.

## How to: (Comment faire : )
En Ruby, on utilise souvent RSpec pour les tests. Voilà le genre de code qu'on écrit :

```Ruby
# installation de RSpec
# gem install rspec

# Exemple de test
require 'rspec'

describe 'Calculatrice' do
  it 'additionne deux nombres' do
    expect(2 + 2).to eq(4)
  end

  it 'soustrait deux nombres' do
    expect(5 - 3).to eq(2)
  end
end
```

Exécutez les tests avec `rspec votre_fichier_spec.rb`. Vous devriez voir quelque chose comme ça si tout est au vert :

```
2 examples, 0 failures
```

## Deep Dive (Plongée profonde)
Les tests automatisés existent depuis que les devs réalisent qu'ils recyclent plus leur code qu'ils ne recyclent leur verre. Alternatives ? Minitest en est une autre bien connue en Ruby. Ça s'utilise à peu près pareil mais c'est plus minimaliste, moins de fioritures. Pourquoi RSpec alors ? Il est expressif, lisible, et donc plus facile à maintenir.

## See Also (Voir aussi)
- Un bon tuto RSpec : [https://www.betterspecs.org/](https://www.betterspecs.org/)
