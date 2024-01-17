---
title:                "Convertir une chaîne en minuscules"
html_title:           "Ruby: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

### Qu'est-ce que c'est et pourquoi?

La conversion d'une chaîne de caractères en minuscules est un processus qui consiste à transformer tous les caractères en minuscules. Les programmeurs font cela pour faciliter la comparaison de chaînes de caractères et pour uniformiser le format des données dans leur code.

### Comment faire:

Voici un exemple de code pour convertir une chaîne de caractères en minuscules en utilisant la méthode `downcase` en Ruby:

```Ruby
my_string = "BONJOUR"
puts my_string.downcase
```

Cela produira une sortie `bonjour` en minuscules.

### Plongée en profondeur:

La conversion de chaînes de caractères en minuscules est une pratique courante dans la programmation. Cela vient de l'histoire de l'écriture des ordinateurs où seules les majuscules étaient initialement disponibles. Il existe également d'autres méthodes pour convertir une chaîne de caractères en minuscules en utilisant des bibliothèques externes telles que `unicode_utils`. Cependant, la méthode `downcase` est la plus simple et la plus couramment utilisée en Ruby car elle ne nécessite pas l'installation de bibliothèques supplémentaires.

### Voir aussi:

- [Documentation officielle de Ruby pour la méthode downcase](https://ruby-doc.org/core-3.0.1/String.html#method-i-downcase)
- [La bibliothèque ruby-unicode](https://github.com/lang/ruby-unicode) pour des méthodes plus avancées de manipulation de chaînes de caractères.