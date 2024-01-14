---
title:    "Ruby: Écriture de tests"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/writing-tests.md"
---

{{< edit_this_page >}}

# Pourquoi écrire des tests en Ruby ?

La rédaction de tests unitaires est essentielle pour un code propre et fiable en Ruby. En écrivant des tests, vous vous assurez que votre code fonctionne correctement, vous permettant ainsi de détecter et de corriger les erreurs avant qu'elles ne deviennent de gros problèmes.

## Comment écrire des tests en Ruby

Il existe plusieurs frameworks pour écrire des tests en Ruby, tels que RSpec ou MiniTest. Voici un exemple de test en utilisant RSpec :

```ruby
require 'rspec'

# Définition de la méthode "add" pour ajouter deux nombres
def add(a, b)
  a + b
end

# Spécifications pour la méthode "add"
RSpec.describe 'add' do
  it 'ajoute deux nombres' do
    expect(add(3, 7)).to eq(10)
  end
  
  it 'retourne le bon résultat avec des nombres négatifs' do
    expect(add(-5, 2)).to eq(-3)
  end
end
```

Dans cet exemple, nous définissons une méthode "add" qui ajoute deux nombres. Puis, nous utilisons RSpec pour tester cette méthode en vérifiant si elle retourne le bon résultat pour différentes entrées. Avec ce genre de tests, nous pouvons être sûrs que notre code fonctionne correctement.

## Une plongée plus profonde

En écrivant des tests en Ruby, vous pouvez également utiliser des mocks et des stubs pour simuler des objets et des interactions avec d'autres parties de votre code. Cela permet de tester des parties spécifiques de votre code sans dépendre de systèmes externes. Les tests vous permettent également de documenter votre code et de faciliter sa maintenance en cas de modifications futures.

# Voir aussi

- [Documentation RSpec](https://rspec.info)
- [Documentation MiniTest](https://github.com/seattlerb/minitest)
- [Article sur l'importance des tests en Ruby](https://medium.com/@adrianstelmasik/why-testing-is-important-for-ruby-projects-8e83dac177a0)