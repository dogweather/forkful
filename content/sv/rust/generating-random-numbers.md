---
title:    "Rust: Generering av slumpmässiga tal"
keywords: ["Rust"]
---

{{< edit_this_page >}}

# Varför Generera Slumpmässiga Nummer?

Att generera slumpmässiga nummer är en vanlig uppgift inom programmering, oavsett vilket språk du arbetar med. Det används ofta inom spelutveckling, datavetenskapliga experiment och andra situationer där slumpmässighet behövs. I den här bloggposten kommer vi titta på hur man kan generera slumpmässiga nummer i Rust.

## Så Här Gör Du

För att generera slumpmässiga nummer i Rust, behöver du använda en funktion från standardbiblioteket som heter `rand`. För att använda denna funktion, behöver vi först importera den med följande kod:

```
use rand::Rng;
```

Nu kan vi använda `rand::Rng` och dess metoder för att generera slumpmässiga nummer. Här är ett exempel:

```
let mut rng = rand::thread_rng();
println!("Ett slumpmässigt nummer: {}", rng.gen::<i32>());
```

I exemplet ovan använder vi `thread_rng()` för att initialisera en instans av `Rng` som vi kan använda för att generera slumpmässiga nummer. Sedan använder vi metoden `gen()` för att generera ett slumpmässigt heltal av typen `i32`. Detta kan enkelt anpassas för att generera andra numeriska typer.

Om vi vill generera ett slumpmässigt tal inom ett visst intervall, kan vi använda `gen_range()` metoden. Här är ett exempel där vi genererar ett tal mellan 1 och 100:

```
let mut rng = rand::thread_rng();
println!("Ett slumpmässigt tal mellan 1 och 100: {}", rng.gen_range(1,101));
```

## Djupdykning

Bakom kulisserna använder Rusts `rand` biblioteket Mersenne Twister algoritmen för att generera slumpmässiga nummer. Detta är en mycket vanlig algoritm som används inom många programmeringsspråk. En intressant funktion i `rand` biblioteket är att det också erbjuder en möjlighet att använda en annan algoritm, XorShift, för att generera slumpmässiga nummer genom att använda `XorShiftRng` istället för `thread_rng()`.

En annan viktig aspekt att tänka på när man genererar slumpmässiga nummer är att algoritmen faktiskt inte är helt slumpmässig eftersom den är beroende av en startvärde, kallad seed. I exemplet använder vi `thread_rng()`, vilket automatiskt seedar RNG med ett slumpmässigt värde baserat på operativsystemets interna kryptografiska generator. Om du vill ha mer kontroll över seedet kan du använda `SeedableRng` som låter dig välja ditt eget seedvärde.

## Se Även

- [Rust `rand` biblioteket](https://docs.rs/rand/0.6.5/rand/)
- [Mersenne Twister algoritmen](https://en.wikipedia.org/wiki/Mersenne_Twister)
- [XorShift algoritmen](https://en.wikipedia.org/wiki/Xorshift)

# Se Även

Det finns många andra användbara funktioner och metoder i Rusts `rand` bibliotek som vi inte hann täcka i den här bloggposten. Om du är intresserad av att lära dig mer om att generera slumpmässiga nummer i Rust, rekommenderar vi att du kollar in dokumentationen och experimentar med koden själv. Lycka till med dina framtida slumpmässiga projekt!