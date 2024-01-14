---
title:                "Ruby: Écriture de tests"
simple_title:         "Écriture de tests"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi

L'écriture de tests est une étape importante dans le processus de développement de logiciels. Cela permet de s'assurer que le code fonctionne correctement et qu'il peut être facilement modifié sans créer de bugs. De plus, cela améliore la qualité du code et facilite la collaboration entre les membres de l'équipe.

## Comment faire

Pour écrire des tests en Ruby, il est recommandé d'utiliser le framework de test RSpec. Il offre une syntaxe facile à comprendre et à utiliser, ainsi qu'une bonne organisation pour les tests. Voici un exemple de test avec RSpec :

```Ruby
# code à tester

def add(x, y)
  return x + y
end

# test avec RSpec

describe "add" do
  it "should add two numbers correctly" do
    result = add(2, 3)
    expect(result).to eq(5)
  end
  
  it "should return the correct type" do
    result = add(2, 3)
    expect(result).to be_a(Integer)
  end
end
```

Lors de l'exécution des tests avec RSpec, vous obtiendrez un résultat comme ceci :

```
Finished in 0.001 seconds (files took 0.31116 seconds to load)
2 examples, 0 failures
```

Cela signifie que tous les tests ont réussi et qu'aucun bug n'a été détecté.

## Plongée en profondeur

Lors de l'écriture de tests, il est important d'avoir une bonne couverture de code. Cela signifie que tous les aspects du code doivent être testés afin de s'assurer qu'il n'y ait pas de bugs. Pour cela, il est recommandé de pratiquer le TDD (Test Driven Development), ce qui signifie écrire les tests avant même d'écrire le code. Cela vous permet de réfléchir à la façon dont votre code devrait fonctionner avant de l'écrire.

Il est également important de tester les cas limites, c'est-à-dire les situations qui peuvent causer des erreurs dans votre code. Par exemple, si votre fonction attend un nombre entier en entrée, il est important de tester ce qui se passe si une chaîne de caractères est passée en paramètre.

## Voir aussi

- [Tutoriel RSpec](https://www.rubyguides.com/2018/07/rspec-tutorial/)
- [Pratiques de développement agiles](https://www.smashingmagazine.com/2018/07/agile-test-driven-development-rspec-capybara/)
- [Documentation RSpec](https://rspec.info/documentation/)