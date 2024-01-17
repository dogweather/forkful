---
title:                "Utiliser les expressions régulières"
html_title:           "Ruby: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c’est & pourquoi?
Les expressions régulières sont des séquences de caractères utilisées pour rechercher et manipuler du texte selon un motif pré-défini. Les programmeurs les utilisent pour effectuer des tâches telles que la validation des entrées de l'utilisateur, le filtrage de données et la recherche de chaînes de caractères spécifiques dans un grand ensemble de données.

## Comment faire:
Les expressions régulières sont écrites avec une syntaxe spécifique et peuvent sembler intimidantes au premier abord, mais elles sont en réalité très puissantes et utiles pour résoudre des problèmes courants de traitement de texte. Voyons un exemple simple en utilisant Ruby:

```
texte = "Bonjour! Comment allez-vous?"
patron = /bonjour/i

if patron.match?(texte)
  puts "Salut!"
else
  puts "Je ne peux pas trouver de correspondance."
end
```

Ici, nous utilisons ```/bonjour/i``` pour créer notre expression régulière, en utilisant le «i» pour indiquer que nous voulons ignorer la casse et trouver «Bonjour» dans notre texte, qu'il soit écrit en majuscules ou en minuscules. Ensuite, nous utilisons la méthode ```match?``` pour vérifier si notre expression régulière trouve une correspondance dans notre texte. Si c'est le cas, nous affichons "Salut!", sinon nous affichons un message d'erreur. Dans cet exemple, notre expression régulière a trouvé une correspondance et nous avons donc obtenu la sortie suivante:

```
Salut!
```

## Plongée en profondeur:
Les expressions régulières ont été inventées dans les années 1950 par le scientifique américain Stephen Cole Kleene. Aujourd'hui, elles sont supportées par de nombreux langages de programmation, dont Ruby. Il existe également des alternatives aux expressions régulières, telles que les expressions rationnelles, qui offrent une syntaxe plus simple mais avec moins de fonctionnalités.

Les expressions régulières peuvent sembler compliquées au début, mais avec de la pratique, vous pourrez les utiliser pour effectuer des tâches complexes de manière rapide et efficace. Ruby propose également des méthodes telles que ```gsub``` et ```scan``` qui prennent en charge les expressions régulières et facilitent leur utilisation dans votre code.

## Voir aussi:
Pour en savoir plus sur les expressions régulières en Ruby, voici quelques ressources utiles:

- [Ruby regular expressions cheat sheet](https://www.ruby-lang.org/en/documentation/quickstart/2/)
- [Rubular](https://rubular.com/): un outil pour tester vos expressions régulières en toute simplicité
- [The Ruby regex tutorial](https://www.tutorialspoint.com/ruby/ruby_regular_expressions.htm): un tutoriel complet sur les expressions régulières en Ruby.