---
date: 2024-01-20 17:51:24.388419-07:00
description: "Comment faire : Historiquement, l'interpolation de cha\xEEnes est un\
  \ concept adopt\xE9 par de nombreux langages de programmation, incluant Perl et\
  \ PHP,\u2026"
lastmod: '2024-04-05T22:51:12.262313-06:00'
model: gpt-4-1106-preview
summary: "Historiquement, l'interpolation de cha\xEEnes est un concept adopt\xE9 par\
  \ de nombreux langages de programmation, incluant Perl et PHP, simplifiant la concat\xE9\
  nation."
title: "Interpolation de cha\xEEnes de caract\xE8res"
weight: 8
---

## Comment faire :
```Ruby
prenom = "Marie"
age = 30

# Interpolation avec des guillemets doubles
message = "Bonjour, je m'appelle #{prenom} et j'ai #{age} ans."
puts message
```
Sortie :
```
Bonjour, je m'appelle Marie et j'ai 30 ans.
```

```Ruby
# Utilisation d'expressions
heure = 9
message = "Il est #{heure > 12 ? heure - 12 : heure}h#{'PM' if heure >= 12 else 'AM'}."
puts message
```
Sortie :
```
Il est 9hAM.
```

## Exploration plus profonde
Historiquement, l'interpolation de chaînes est un concept adopté par de nombreux langages de programmation, incluant Perl et PHP, simplifiant la concaténation. En Ruby, elle est particulièrement flexible grâce aux guillemets doubles qui reconnaissent les portions de code entre `#{}` comme du Ruby à évaluer. 
Les alternatives sans interpolation incluent la concaténation avec `+` ou l'usage de la méthode `sprintf` ou `%`. Ces méthodes sont plus verboses et moins intuitives.
Ruby interne convertit le code interpolé en une concaténation de chaînes, optimisée pour la performance.

## Voir aussi
- Ruby Documentation sur l'interpolation : [Ruby-Doc String Interpolation](https://ruby-doc.org/core/String.html#method-i-3C-3C)
- Discussion sur l'interpolation vs concaténation : [Stack Overflow](https://stackoverflow.com/questions/10076579/string-concatenation-vs-interpolation-in-ruby)
