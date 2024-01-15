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

## Pourquoi
Les expressions régulières sont des outils puissants pour la manipulation de chaînes de caractères dans vos programmes Ruby. Elles vous permettent de rechercher, d'extraire et de modifier facilement des données en utilisant des modèles de correspondance.

## Comment utiliser les expressions régulières en Ruby
Les expressions régulières sont définies par des motifs entre des barres obliques ("/"). Par exemple, `/hello/` correspondra à la chaîne "hello" dans une autre chaîne.

Voici un exemple de code Ruby utilisant une expression régulière pour trouver toutes les occurrences de "ruby" dans une chaîne et les remplacer par "Ruby":
```Ruby
sentence = "J'aime écrire en Ruby. Ruby est un langage de programmation polyvalent."
puts sentence.gsub(/ruby/, 'Ruby')
```
La sortie de ce code sera:
```
J'aime écrire en Ruby. Ruby est un langage de programmation polyvalent.
```

Il existe également des raccourcis pour certains motifs couramment utilisés. Par exemple, le motif `/[0-9]/` correspondra à n'importe quel chiffre et le motif `/[a-z]/` correspondra à n'importe quelle lettre minuscule.

Les expressions régulières ont également de nombreuses autres fonctionnalités avancées telles que les groupes de capture, les caractères spéciaux et les modificateurs de correspondance. Il est recommandé de consulter la documentation officielle de Ruby pour en savoir plus.

## Plongée en profondeur
Les expressions régulières peuvent sembler intimidantes à première vue, mais elles peuvent être très utiles une fois que vous les maîtrisez. Voici quelques astuces pour vous aider:

- Utilisez différents sites en ligne pour tester et expérimenter vos expressions régulières en temps réel, tels que Regex101 ou Rubular.
- Vous pouvez utiliser les expressions régulières pour valider des entrées utilisateur telles qu'un numéro de téléphone ou une adresse e-mail.
- En utilisant des groupes de capture, vous pouvez extraire des données spécifiques d'une chaîne et les réutiliser dans d'autres motifs ou dans votre code.
- Les expressions régulières sont également disponibles dans d'autres langages de programmation tels que JavaScript ou Python, ce qui permet une plus grande portabilité de votre code.

## Voir aussi
- [Documentation officielle de Ruby sur les expressions régulières](https://ruby-doc.org/core-2.7.0/Regexp.html)
- [Tutoriel en français sur les expressions régulières en Ruby](https://openclassrooms.com/fr/courses/1302681-introduction-aux-expressions-regulieres)
- [Site de pratique d'expressions régulières](https://regex101.com/)