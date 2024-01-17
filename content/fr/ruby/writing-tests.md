---
title:                "Ecrire des tests"
html_title:           "Ruby: Ecrire des tests"
simple_title:         "Ecrire des tests"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Les tests de code sont des morceaux de code écrits pour vérifier si d'autres morceaux de code fonctionnent comme ils le devraient. Les programmeurs effectuent des tests pour s'assurer que leur code est sans erreur et fonctionne correctement. Cela peut aider à prévenir les bugs et à garantir que le code est de qualité.

## Comment faire:

```ruby 
require 'test/unit' 

def additionner(x, y)
    x + y
end

class TestAddition < Test::Unit::TestCase 
    def test_additionner 
        assert_equal(4, additionner(2, 2)) 
    end 
end
```

```ruby
Costumes = ["Batman", "Superman", "Wonder Woman"]

Test::Unit::TestCase 

def setup 
    @costumes = Costumes 
end

def test_heroes 
    assert_equal("Batman", Costumes[0]) 
    assert_equal("Wonder Woman", Costumes[2]) 
end 
```

### Résultat de l'exécution:

Exécutez vos tests en utilisant la commande `ruby nom_du_fichier_test.rb`.

![Test results output](link_to_image)

## Deep Dive:

Historiquement, les tests étaient écrits après le code, mais de nos jours, en utilisant des méthodes telles que le développement piloté par les tests (TDD), les tests sont écrits avant le code. Cela peut aider à créer un code plus modulaire et plus facile à maintenir. Il existe également des alternatives aux tests unitaires, telles que les tests d'intégration et les tests fonctionnels.

## See Also:

Pour plus d'informations sur l'écriture de tests en Ruby, consultez le [Guide des tests en Ruby](link_to_guide) et le [Guide TDD en Ruby](link_to_TDD_guide).