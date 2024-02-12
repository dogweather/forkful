---
title:                "Generera slumptal"
aliases:
- /sv/php/generating-random-numbers.md
date:                  2024-01-27T20:35:04.729292-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generera slumptal"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att generera slumpmässiga nummer i PHP handlar om att producera oförutsägbara värden inom ett angivet intervall, vilket är väsentligt för uppgifter som att skapa unika användar-ID, generera lösenord eller för användning i simuleringar och spel. Programmerare förlitar sig på slumpmässighet för att lägga till oförutsägbarhet och variabilitet i sina applikationer, vilket gör processer som testning eller användarupplevelser mer robusta och engagerande.

## Hur man gör:

PHP erbjuder flera funktioner för att generera slumpmässiga nummer, men de vanligast använda är `rand()`, `mt_rand()`, och för kryptografiska ändamål, `random_int()`.

För att generera ett enkelt slumpmässigt nummer mellan 0 och getrandmax() (det största möjliga värdet som returneras av `rand()`), kan du använda:

```PHP
echo rand();
```

För ett mer specifikt intervall, till exempel mellan 1 och 100:

```PHP
echo rand(1, 100);
```

Dock är `mt_rand()` ett bättre val för hastighet och slumpmässighet:

```PHP
echo mt_rand(1, 100);
```

Utdata för båda kan vara något mellan 1 och 100, beroende på slumpmässigheten, t.ex., `42`.

För kryptografiska eller säkerhetsmässiga sammanhang, där oförutsägbarhet är avgörande, är `random_int()` det föredragna valet eftersom den genererar kryptografiskt säkra pseudo-slumpmässiga heltal:

```PHP
echo random_int(1, 100);
```

Återigen är utdatan ett slumpmässigt nummer mellan 1 och 100, som `84`, men med en starkare garanti för slumpmässighet.

## Fördjupning

Funktionen `rand()` har funnits i PHP sedan dess tidiga versioner och fungerade som den ursprungliga metoden för att generera slumpmässiga nummer. Dock är den inte det bästa valet för applikationer som kräver en hög grad av slumpmässighet på grund av dess relativt förutsägbara algoritm.

`mt_rand()`, introducerad i PHP 4, baseras på Mersenne Twister-algoritmen - långt överlägsen i termer av hastighet och den slumpmässighet den kan generera jämfört med `rand()`. Den blev snabbt det föredragna alternativet för de flesta icke-kryptografiska behoven.

För säkerhetskänsliga applikationer introducerades `random_int()` i PHP 7 för att generera kryptografiskt säkra pseudo-slumpmässiga heltal med hjälp av slumpmässiga byte från systemets slumpgenerator. Det är betydligt säkrare än `rand()` eller `mt_rand()`, vilket gör det till det bästa valet för att generera tokens, nycklar eller andra element där förutsägbarhet kan leda till säkerhetsrisker.

Trots dessa förbättringar är det avgörande att välja rätt funktion baserat på applikationens sammanhang. För allmänt bruk räcker `mt_rand()`, men för något som kan vara måltavla eller utnyttjas är `random_int()` vägen att gå, och erbjuder både slumpmässighet och säkerhet.
