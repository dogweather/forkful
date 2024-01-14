---
title:    "Ruby: Écrire des tests."
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur de logiciels, vous savez à quel point il est important de s'assurer que notre code fonctionne correctement. La meilleure façon de garantir cela est d'écrire des tests. Les tests nous aident à détecter les erreurs dès le début du processus de développement et à nous assurer que notre code continue de fonctionner correctement au fur et à mesure que nous apportons des modifications. Cela nous fait gagner du temps et nous permet de fournir un code de meilleure qualité à nos utilisateurs. 

## Comment faire

Ecrire des tests en Ruby est assez simple. Nous allons vous montrer comment le faire en utilisant l'outil de test par défaut, MiniTest. Voici un exemple de test pour une classe simple "Car" :

```Ruby
require 'minitest/autorun'

class CarTest < Minitest::Test
  def setup
    @car = Car.new('Ferrari', 'Red')
  end

  def test_get_name
    assert_equal 'Ferrari', @car.name
  end

  def test_get_color
    assert_equal 'Red', @car.color
  end
end
```

Dans cet exemple, nous importons le framework de tests MiniTest et créons une classe de test pour notre classe "Car". Dans la méthode "setup", nous initialisons une instance de la classe "Car" avec une marque et une couleur. Ensuite, nous écrivons deux méthodes de test pour vérifier que notre code fonctionne correctement. La méthode "assert_equal" vérifie si la valeur donnée est égale à la valeur attendue. Si les deux valeurs sont égales, le test passe.

## Plongée profonde

Maintenant que nous savons comment écrire des tests en Ruby, il est important de comprendre comment écrire des tests efficaces. Voici quelques bonnes pratiques à suivre :

- Écrivez des tests dès le début du processus de développement pour détecter les erreurs le plus tôt possible.
- Testez toutes les fonctionnalités de votre code, même les plus petites.
- Utilisez des données de test réalistes pour vous assurer que vos tests représentent bien les cas d'utilisation réels.
- Utilisez des noms de méthodes et des descriptions de test clairs et cohérents pour faciliter la lecture et la compréhension des tests ultérieurement.
- Toujours corriger les erreurs détectées par les tests avant de déployer votre code.

## Voir aussi

- [Documentation de MiniTest](https://ruby-doc.org/stdlib-2.6.3/libdoc/minitest/rdoc/MiniTest.html)
- [Guide complet pour écrire des tests en Ruby](https://www.rubyguides.com/2018/07/ruby-testing-minitest/)