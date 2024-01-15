---
title:                "Utiliser les expressions régulières"
html_title:           "Clojure: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

Pourquoi: Les expressions régulières sont un outil puissant et flexible pour manipuler des chaînes de caractères dans vos programmes Clojure. Elles peuvent être utilisées pour extraire des données spécifiques, valider des entrées utilisateur et même transformer des données en masse.

Comment faire: Voici quelques exemples pratiques de l'utilisation des expressions régulières en Clojure:

### Chercher et remplacer une chaîne de caractères

```Clojure
user=> (clojure.string/replace "Bonjour le monde!" #"le" "la")
"Bonjour la monde!"
```

Dans cet exemple, nous utilisons la fonction `replace` de la bibliothèque standard `clojure.string` pour remplacer toutes les occurrences de "le" par "la" dans la chaîne de caractères donnée. Notez que nous avons également utilisé des expressions régulières pour spécifier le motif à remplacer.

### Extraire des informations d'une chaîne de caractères

```Clojure
user=> (re-find #"\d{3}-\d{2}-\d{4}" "Mon numéro de sécurité sociale est 123-45-6789")
"123-45-6789"
```

Ici, nous utilisons la fonction `re-find` de la bibliothèque standard `clojure.core` pour extraire un numéro de sécurité sociale à 9 chiffres d'une chaîne de caractères donnée. L'expression régulière `\d{3}-\d{2}-\d{4}` correspond à un motif de trois chiffres, suivis d'un tiret, puis de deux chiffres, puis d'un autre tiret, puis de quatre chiffres.

### Valider une adresse email

```Clojure
user=> (re-matches #"^[a-z0-9]+@[a-z]+\.[a-z]{2,3}$" "utilisateur@test.com")
true
```

Dans cet exemple, nous utilisons la fonction `re-matches` de la bibliothèque standard `clojure.core` pour valider une adresse email donnée en utilisant une expression régulière. Le motif `^[a-z0-9]+@[a-z]+\.[a-z]{2,3}$` correspond à une chaîne de caractères commençant par des lettres ou des chiffres, suivie d'un symbole "@", puis d'un domaine composé de lettres et se terminant par une extension de deux ou trois lettres.

Plongée en profondeur: Les expressions régulières sont basées sur la syntaxe PCRE (Perl Compatible Regular Expressions) et peuvent être utilisées dans de nombreux langages de programmation, y compris Clojure. Elles sont extrêmement polyvalentes et peuvent être particulièrement utiles pour nettoyer, extraire ou valider des données dans vos programmes. Cependant, il est important de noter que les expressions régulières peuvent être complexes et difficiles à comprendre pour les débutants. Il est donc recommandé de suivre des tutoriels ou d'utiliser des outils de test en ligne pour vous familiariser avec leur syntaxe et leur fonctionnement.

Voir aussi: 
- [Documentation Clojure sur les expressions régulières](https://clojuredocs.org/clojure.core/re-pattern)
- [Exemples et exercices pour pratiquer les expressions régulières](https://regexone.com/)
- [Outil de test en ligne pour les expressions régulières](https://regex101.com/)