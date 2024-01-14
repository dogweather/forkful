---
title:    "Ruby: Användning av reguljära uttryck"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Varför använda reguljära uttryck inom Ruby?

Att lära sig använda reguljära uttryck kan vara en kraftfull verktyg för att göra mer avancerad strängmatchning i Ruby. Det kan hjälpa dig att hitta mönster, ersätta delar av strängar och mycket mer. Det är också en vanlig teknik som används inom andra programmeringsspråk, vilket gör det användbart att kunna inom flera olika områden.

## Hur man använder reguljära uttryck inom Ruby

För att använda reguljära uttryck inom Ruby behöver du en förståelse för olika metakaraktärer och hur man kombinerar dem för att skapa mönster. Här är ett enkelt exempel på hur man hittar ett specifikt ord i en sträng och ersätter det:

```Ruby
str = "Hej, mitt namn är Ruby!"
str.gsub!(/Ruby/, "Python")
puts str
```

Detta kommer att ge utmatningen "Hej, mitt namn är Python!" Den första delen av koden skapar en sträng som vi vill utföra matchning och ersättning på. Sedan använder vi `gsub!` metoden för att ersätta alla instanser av "Ruby" med "Python". Om vi istället bara ville byta ut den första instansen av "Ruby" kan vi använda `sub!` istället.

Reguljära uttryck kan också användas för att hitta mönster i strängar, till exempel om en sträng innehåller ett visst antal siffror:

```Ruby
str = "Du har fått 5 nya meddelanden."
if str[/(\d+) nya/]
  puts "Det finns #{$1} nya meddelanden."
end
```

Detta kommer att ge utmatningen "Det finns 5 nya meddelanden." Här använder vi `[]` metoden för att matcha strängen mot vårt mönster, som är ett eller flera siffror (`\d+`) följt av "nya". Den del av strängen som matchas sparas sedan i variabeln `$1`.

## Djupdykning i reguljära uttryck

Det finns många olika metakaraktärer och mönster som kan användas i reguljära uttryck, vilket gör dem väldigt mångsidiga. Här är några exempel på metakaraktärer som kan vara användbara att känna till:

- `.` - matchar ett enda tecken
- `*` - matchar noll eller flera instanser av det föregående mönstret
- `+` - matchar en eller flera instanser av det föregående mönstret
- `\d` - matchar en siffra
- `^` - matchar början av en sträng
- `$` - matchar slutet av en sträng
- `()` - skapar en grupp för att spara matchade delar av strängen

Det finns också olika modifierare som kan användas för att göra reguljära uttryck mer flexibla, till exempel `i` för att ignorera skiftlägeskänslighet och `m` för flerradsmatchning.

Det finns mycket mer att lära sig om reguljära uttryck, men dessa grundläggande exempel bör hjälpa dig att komma igång. Tveka inte att utforska fler mönster och experimentera med dem för att få en ännu bättre förståelse för hur de fungerar.

## Se även

- [Reguljära uttryck i Ruby](https://ruby-doc.org/core-2.7.2/Regexp.html)
- [Reguljära uttryck Cheat Sheet](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Utmärkt Regex](https://www.excel-egenvard.com/) (en sida som hjälper till att visualisera och testa reguljära uttryck)