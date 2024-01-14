---
title:    "Ruby: Conversion d'une chaîne en minuscules"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Pourquoi
Il est souvent utile et pratique de convertir une chaîne de caractères en lettres minuscules dans un programme Ruby. Cela peut faciliter la recherche de mots-clés, l'affichage cohérent de données et bien plus encore.

## Comment faire
Pour effectuer cette conversion, il suffit d'utiliser la méthode `.downcase` sur la variable contenant la chaîne de caractères souhaitée. Voici un exemple de code:

```Ruby
texte = "JE SUIS UNE CHAÎNE DE CARACTÈRES"

puts texte.downcase
```

Et voici le résultat attendu:

```
je suis une chaîne de caractères
```

Cette méthode est également utile pour comparer des chaînes de caractères en ignorant les différences de casse. Par exemple:

```Ruby
texte1 = "Bonjour"
texte2 = "BONJOUR"

puts texte1.downcase == texte2.downcase
```
Le résultat sera `true`, car les deux chaînes ont été converties en lettres minuscules avant la comparaison.

## Plongée plus profonde
En plus de la méthode `.downcase`, Ruby propose également la méthode `.downcase!` qui modifie directement la variable contenant la chaîne de caractères plutôt que de créer une nouvelle chaîne. Par exemple:

```Ruby
texte = "JE SUIS UNE CHAÎNE DE CARACTÈRES"

texte.downcase!

puts texte
```
Le résultat sera:

```
je suis une chaîne de caractères
```

Il est également important de noter que la méthode `.downcase` ne fonctionne que sur les caractères ASCII. Si votre chaîne de caractères contient des caractères spéciaux ou des lettres accentuées, ils ne seront pas convertis en minuscules. Pour cela, il est nécessaire d'utiliser la méthode `.normalize` en plus de la méthode `.downcase`.

## Voir aussi
- [Documentation officielle de Ruby sur les méthodes de chaînes de caractères](https://ruby-doc.org/core-2.7.3/String.html#method-i-downcase)
- [Article de blog Medium sur la manipulation de chaînes de caractères en Ruby](https://medium.com/ignitionweekly/ruby-strings-string-manipulation-in-ruby-b43a6c7357a7)