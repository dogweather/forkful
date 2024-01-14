---
title:    "Ruby: Démarrer un nouveau projet"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

##Pourquoi

Si vous êtes intéressé par le développement de logiciels, vous vous demandez peut-être pourquoi vous devriez vous lancer dans un nouveau projet en Ruby. Eh bien, la réponse est simple : Ruby est un langage de programmation puissant, élégant et facile à apprendre, ce qui en fait un choix idéal pour créer des applications robustes et dynamiques.

##Comment faire

Maintenant que vous êtes convaincu de créer un nouveau projet en Ruby, voici quelques exemples de code pour vous aider à démarrer :

```Ruby
# Définition d'un simple tableau contenant des prénoms
prenoms = ["Emma", "Lucas", "Louise", "Adam"]

# Afficher les prénoms un par un
prenoms.each do |prenom|
  puts prenom
end

# Définition d'une classe "Compte" avec une méthode "solde" et deux attributs : "nom" et "solde_initial"
class Compte
  attr_accessor :nom, :solde
  def initialize(nom, solde_initial)
    @nom = nom
    @solde = solde_initial
  end
end

# Création d'un compte et affichage de son solde
compte_1 = Compte.new("John", 1000)
puts "Le solde du compte de #{compte_1.nom} est de #{compte_1.solde} euros."
```

Résultat :

```
Emma
Lucas
Louise
Adam
Le solde du compte de John est de 1000 euros.
```

##Plongée en profondeur

Maintenant que vous avez une idée de la syntaxe de base de Ruby, vous pouvez approfondir vos connaissances en explorant des concepts tels que les classes, les méthodes, les boucles et les conditions. Vous pouvez également utiliser des bibliothèques populaires telles que Rails pour développer des applications web ou RSpec pour écrire des tests automatisés.

N'oubliez pas d'expérimenter et de consulter la documentation officielle de Ruby pour découvrir toutes les fonctionnalités intéressantes que ce langage a à offrir.

##Voir aussi

- [La documentation officielle de Ruby](https://www.ruby-lang.org/fr/documentation/)
- [Le site officiel de Ruby on Rails](https://rubyonrails.org/)
- [La documentation officielle de RSpec](http://rspec.info/)

Maintenant que vous avez les bases pour commencer un nouveau projet en Ruby, il est temps de plonger et de créer quelque chose d'incroyable ! Bonne chance !