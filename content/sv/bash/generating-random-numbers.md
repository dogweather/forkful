---
title:    "Bash: Generering av slumpmässiga tal"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Varför
Att generera slumpmässiga tal är en vanlig uppgift inom programmering, och det kan ha många olika användningsområden. Det kan till exempel vara användbart vid simuleringar, för att skapa slumpmässig data för testning eller för att skapa unika identifierare.

## Så här gör du
För att generera slumpmässiga tal i Bash, kan du använda inbyggda funktionen "RANDOM". Detta kommer att returnera ett heltal mellan 0 och 32767.

```Bash
echo $RANDOM
```

Om du vill begränsa intervallet för de slumpmässiga talen, kan du använda modulusoperatorn "%" för att få ett tal inom ett visst intervall. I exemplet nedan kommer det slumpmässiga talet att vara mellan 1 och 10.

```Bash
echo $((RANDOM % 10 + 1))
```

Du kan också använda "shuf" -kommandot för att generera en lista med slumpmässiga tal. Detta är särskilt användbart om du vill ha flera slumpmässiga tal utan att behöva skriva en loop.

```Bash
shuf -i 1-10 -n 5
```

Ovanstående exempel kommer att skriva ut 5 slumpmässiga tal mellan 1 och 10.

## Djupdykning
Bash innehåller enkla funktioner för att generera slumpmässiga tal, men det kan också vara användbart att förstå hur dessa tal faktiskt genereras. Bakom kulisserna använder "RANDOM" -funktionen en teknik som kallas för pseudo-slumpmässig algoritm, vilket innebär att det inte är helt slumpmässigt utan baseras på en seed som bestämmer vilka tal som genereras. Om du till exempel använder samma seed kommer "RANDOM" att generera samma serie av slumpmässiga tal varje gång.

För att förbättra slumpmässigheten när du använder "shuf", kan du ange en annan seed eller en annan slumpmässig faktor genom att använda "-r" flaggan. Detta kommer att ge ett mer varierat resultat varje gång du kör kommandot.

## Se även
- [Generera slumpmässiga tal i Bash](https://www.linuxjournal.com/content/generating-random-numbers-bash)
- [shuf dokumentation](https://www.gnu.org/software/coreutils/manual/html_node/shuf-invocation.html)
- [RANDOM dokumentation](https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html#index-RANDOM)