---
title:                "Stor bokstavering av en sträng"
html_title:           "Ruby: Stor bokstavering av en sträng"
simple_title:         "Stor bokstavering av en sträng"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kapitalisera en sträng är en vanlig manipulation i programmering, och det kan vara användbart för att exempelvis skapa korrekt formaterad output eller för att underlätta jämförelser mellan textsträngar.

## Så här gör du

För att kapitalisera en sträng i Ruby, använd metoden `capitalize` på strängen. Den här metoden kommer att göra första bokstaven i strängen stor och resten av bokstäverna små. Här är ett exempel på hur man kapitaliserar en sträng i Ruby:

```Ruby
sträng = "hej, jag är en sträng"
puts sträng.capitalize
```

Detta kommer att ge följande output:

```Ruby
"Hej, jag är en sträng"
```

Som du kan se har den första bokstaven blivit stor medan resten av bokstäverna är små.

Du kan också använda metoden `capitalize!` på en sträng för att ändra den ursprungliga strängen istället för att skapa en ny. Här är ett exempel på hur man använder `capitalize!`:

```Ruby
sträng = "jag är en annan sträng"
sträng.capitalize!
puts sträng
```

Detta kommer att ge samma output som tidigare exempel, men den här gången kommer den ursprungliga strängen att ändras efter att `capitalize!` metoden har tillämpats på den.

## Djupdykning

En intressant detalj med `capitalize` metoden är att den bara gör första bokstaven i en sträng stor, oavsett hur många ord eller tecken som finns i strängen. Om du till exempel har en sträng som består av flera ord, kommer bara första bokstaven i första ordet att bli stor.

Detta kan bli ett problem om du vill att varje ord i en sträng ska börja med stor bokstav. I så fall behöver du använda metoden `titleize` istället, som kapitaliserar varje ord i en sträng. Här är ett exempel på hur man använder `titleize`:

```Ruby
sträng = "hej, det här är en sträng"
puts sträng.titleize
```

Output kommer att bli:

```Ruby
"Hej, Det Här Är En Sträng"
```

Som du kan se har nu varje ord i strängen blivit kapitaliserade, vilket kanske är ett mer önskat resultat i vissa fall.

## Se även

- [Ruby String Documentation](https://ruby-doc.org/core-2.7.0/String.html)
- [Ruby capitalize method documentation](https://ruby-doc.org/core-2.7.0/String.html#method-i-capitalize)
- [Ruby titleize method documentation](https://apidock.com/rails/String/titleize)