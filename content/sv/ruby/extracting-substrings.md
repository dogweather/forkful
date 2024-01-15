---
title:                "Utdragning av substränger"
html_title:           "Ruby: Utdragning av substränger"
simple_title:         "Utdragning av substränger"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför
Varför skulle man vilja extrahera substrängar i Ruby? Det kan vara användbart när du behöver isolera en del av en sträng för att manipulera eller analysa den på ett specifikt sätt.

## Hur man gör
För att extrahera en substräng i Ruby kan du använda metoden `slice` eller `[]` på en sträng med de index som motsvarar den del du vill extrahera. Här är ett exempel som visar hur man kan extrahera de första fyra bokstäverna i en sträng:

```Ruby
name = "Johanna"
puts name[0..3] #=> "Joha"
puts name.slice(0, 4) #=> "Joha"
```

Om du vill extrahera från en viss position i strängen kan du också använda `slice` med ett utropstecken före indexet. Det här exemplet visar hur man kan extrahera allt efter den tredje bokstaven:

```Ruby
address = "Stockholm"
puts address[3..-1] #=> "kholm"
puts address.slice(3..-1) #=> "kholm"
```

## Djupdykning
När du extraherar substrängar bör du vara medveten om att Ruby använder 0-indexing, vilket innebär att det första tecknet i en sträng har index 0. Om du försöker extrahera en del av strängen med ett högre index än det sista tecknet, kommer du inte få någon utdata tillbaka.

Du kan också använda `slice` med bara ett index för att extrahera en singel bokstav, och du kan även använda negativa index för att räkna bakifrån. Här är ett exempel som visar detta:

```Ruby
word = "programmering"
puts word[-5..-1] #=> "ering"
puts word.slice(-5, 5) #=> "ering"
puts word[1] #=> "r"
```

En annan användbar funktion när man extraherar substrängar är `split`. Den delar upp en sträng i en array baserad på ett mellanslag eller annan vald karaktär. Här är ett exempel som delar upp en sträng baserad på mellanslag och sedan extraherar en del av den:

```Ruby
sentence = "Jag älskar att programmera"
words = sentence.split(" ")
puts words[2] #=> "att"
```

## Se även
* [Ruby-docs: String](https://ruby-doc.org/core-2.7.0/String.html)
* [Ruby-docs: Array](https://ruby-doc.org/core-2.7.0/Array.html)
* [Ruby-docs: Enumerable](https://ruby-doc.org/core-2.7.0/Enumerable.html)