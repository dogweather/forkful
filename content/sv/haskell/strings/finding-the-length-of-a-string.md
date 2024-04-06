---
date: 2024-01-20 17:47:39.979624-07:00
description: "S\xE5 h\xE4r g\xF6r du: Haskell \xE4r enkelt. Anv\xE4nd `length` funktionen\
  \ p\xE5 en str\xE4ng f\xF6r att f\xE5 dess l\xE4ngd. H\xE4r \xE4r ett exempel."
lastmod: '2024-04-05T22:37:46.643488-06:00'
model: gpt-4-1106-preview
summary: "Haskell \xE4r enkelt. Anv\xE4nd `length` funktionen p\xE5 en str\xE4ng f\xF6\
  r att f\xE5 dess l\xE4ngd. H\xE4r \xE4r ett exempel."
title: "Hitta l\xE4ngden p\xE5 en str\xE4ng"
weight: 7
---

## Så här gör du:
Haskell är enkelt. Använd `length` funktionen på en sträng för att få dess längd. Här är ett exempel:

```haskell
main :: IO ()
main = print (length "Hej Sverige!")
```

Exempel på output:

```
12
```

## Fördjupning
`length` funktionen i Haskell har sina rötter i funktionell programmering, där operationer på listor är grundläggande. Strängar hanteras som listor av tecken och `length` ger helt enkelt antalet element.

Det finns alternativ till `length`. Du kan skriva en rekursiv funktion för att räkna tecken eller använda `foldr` för att ackumulera längden. Men `length` är standard och effektiv.

Implementationen av `length` är optimerad men får kritik för att vara långsam med mycket stora listor, då den måste gå igenom varje element.

## Se även
- [Haskell Language Documentation](https://www.haskell.org/documentation/)
- ["Learn You a Haskell for Great Good!" av Miran Lipovača](http://learnyouahaskell.com/)
