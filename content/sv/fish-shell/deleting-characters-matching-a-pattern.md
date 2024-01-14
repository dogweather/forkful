---
title:    "Fish Shell: Att ta bort tecken som matchar ett mönster"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför
Att ta bort tecken som matchar ett mönster är en användbar funktion i Fish Shell, speciellt för att rensa upp data och filtrera resultat.

## Så här gör du
För att ta bort tecken som matchar ett mönster i Fish Shell, används kommandot `string-match`. Detta kommando tar ett mönster som argument och matchar det mot en sträng. Därefter kan man använda `string sub` kommandot för att ta bort matchande tecken från strängen.

```
Fish Shell kodblock:

set str "Hej, jag heter Anna!"
string-match -r "a" $str
string sub -r "a" "" $str
```

Resultatet av ovanstående kodblock skulle vara `Hj, jg het Ann!`, där alla förekomster av bokstaven "a" har tagits bort från strängen.

Man kan också använda flaggan `-qv` tillsammans med `string-match`, som står för "quiet" och "invert". Detta innebär att kommandot kommer ignorera matchande tecken och bara förändra de som inte matchar mönstret.

```
Fish Shell kodblock:

string-match -vq -r "a" $str
string sub -r "a" "" $str
```

I ovanstående exempel skulle resultatet bli `a, heer Ann!`, där enbart bokstaven "a" har tagits bort från strängen.

## Djupdykning
När man använder `string-match` för att ta bort matchande tecken, kan man använda olika flaggor för att ändra beteendet. Här är några användbara flaggor:

- `-i`: Matcha inte storleksförändringar, exempelvis "a" skulle också matcha "A".
- `-r`: Använd ett reguljärt uttryck för att matcha mönstret.
- `-q`: Tystlägg utskrifter och bara returnera resultatet.
- `-v`: Invertera matchningen och ignorera matchande tecken.

Det finns också olika sätt att anpassa mönstret som används för att matcha tecken. Man kan använda metakaraktärer, som "*a" för att matcha alla bokstäver i en sträng som slutar på "a", eller "[abc]" för att matcha alla tecken i strängen som antingen är "a", "b" eller "c". Det finns många andra metakaraktärer som kan användas för att bygga mer avancerade mönster. Det är värt att experimentera med olika mönster för att se vad som fungerar bäst i ens specifika användningsfall.

## Se även
- [Fish Shell officiell hemsida](https://fishshell.com)
- [Fish Shell tutorials](https://fishshell.com/docs/current/tutorial.html)
- [Officiell snabbguide för Fish Shell](https://fishshell.com/docs/current/tutorial.html)