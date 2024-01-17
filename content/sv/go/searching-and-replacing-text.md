---
title:                "Söka och byta ut text"
html_title:           "Go: Söka och byta ut text"
simple_title:         "Söka och byta ut text"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?

Söka och ersätta text är en vanlig uppgift för programmerare. Det innebär att man letar efter ett visst mönster av text och byter ut det med ett annat. Detta är användbart för att uppdatera och ändra kod på ett effektivt sätt.

## How to:

Go tillhandahåller flera inbyggda funktioner för att söka och ersätta text. Se nedan för exempel av dessa funktioner.

```
// Byt ut alla förekomster av "Hello" med "Hej" i texten "Hello World"
text := "Hello World"
nyaText := strings.Replace(text, "Hello", "Hej", -1)
fmt.Println(nyaText) 
// Output: Hej World

// Söka efter ett visst mönster i en sträng och byta ut det med en annan sträng
text := "Jag älskar att programmera i Go!"
regex := regexp.MustCompile("älskar")
nyaText := regex.ReplaceAllString(text, "tycker om")
fmt.Println(nyaText)
// Output: Jag tycker om att programmera i Go!
```

## Deep Dive:

I de tidigaste stadierna av programmering, var sökning och ersättning en mycket manuell process som krävde att programmerare gick igenom och ändrade varje förekomst av en viss sträng. Men med utvecklingen av moderna språk och bibliotek, har detta blivit en mycket enklare uppgift.

Alternativ till Go's inbyggda funktioner är tredjepartsbibliotek såsom "strings" och "regexp" som erbjuder avancerade mönstermatchningsfunktioner för att söka och ersätta text. Det finns också andra språk som Python och Perl som är kända för sina starka sökning och ersättningsfunktioner.

Implementationen av sökning och ersättning funktioner i Go är baserad på Unicode-teknik för att hantera olika språk och teckenuppsättningar. Det finns också möjligheter att använda modifierare för att göra sökningen mer flexibel.

## See Also:

- [Go's strings Package](https://pkg.go.dev/strings)
- [Go's regexp Package](https://pkg.go.dev/regexp)
- [Python's re Module](https://docs.python.org/3/library/re.html)
- [Perl Regular Expressions](https://perldoc.perl.org/perlre)