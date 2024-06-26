---
date: 2024-01-20 17:51:50.722105-07:00
description: "Hur man g\xF6r: Interpolering g\xF6rs med `#{}` inuti en \"dubbelciterad\"\
  \ str\xE4ng. H\xE4r \xE4r n\xE5gra exempel."
lastmod: '2024-03-13T22:44:38.414674-06:00'
model: gpt-4-1106-preview
summary: "Interpolering g\xF6rs med `#{}` inuti en \"dubbelciterad\" str\xE4ng."
title: "Interpolera en str\xE4ng"
weight: 8
---

## Hur man gör:
Interpolering görs med `#{}` inuti en "dubbelciterad" sträng. Här är några exempel:

```Ruby
namn = 'Gustav'
hälsning = "Hej, #{namn}!"
puts hälsning  # => Hej, Gustav!

tid = Time.now
tid_meddelande = "Klockan är nu #{tid.strftime('%H:%M')}."
puts tid_meddelande  # => Klockan är nu 14:30.

summa = 10 + 15
meddelande = "Resultat: #{summa}. Bra jobbat!"
puts meddelande  # => Resultat: 25. Bra jobbat!
```

## Fördjupning
Stringinterpolering har funnits i Ruby sedan det första publika releasen 1995. Det är bättre än konkatenation (`+`) för prestanda och läsbarhet. När du interpolerar, skapas ingen ny sträng förrän det är dags att utvärdera den fullständiga strängen. Metoden `to_s` anropas på varje objekt inom interpoleringskoden, så du kan stoppa nästan vad som helst inne i `#{}`.

Alternativ till interpolering inkluderar strängkonkatenering och `sprintf` eller `%`-notationen. Men interpolering är vanligtvis renare:

```Ruby
klassiker = "pi = " + Math::PI.to_s
puts klassiker  # => pi = 3.141592653589793

med_sprintf = "pi = %f" % Math::PI
puts med_sprintf  # => pi = 3.141593

interpolerat = "pi = #{Math::PI}"
puts interpolerat  # => pi = 3.141592653589793
```

## Se även
- Ruby's dokumentation om strängar: https://ruby-doc.org/core-2.7.0/String.html
- Ett bra Ruby tutorials för nybörjare: https://www.learnrubyonline.org/
- Ruby Style Guide rekommenderar interpolering över konkatenation: https://rubystyle.guide/#string-interpolation
