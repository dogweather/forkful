---
title:    "Ruby: Generera slumpmässiga tal"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer är en viktig del av många olika programmeringsuppgifter. Det kan vara användbart för datorspel, simuleringsprogram eller kryptografi.

## Så här gör du

För att generera slumpmässiga nummer i Ruby kan du använda inbyggda metoder som rand() eller srand(). Till exempel kan du skapa en enkel kodblock som genererar och skriver ut tre slumpmässiga heltal mellan 1 och 10:

```Ruby
3.times do
    puts rand(1..10)
end
```

Detta kodblock kommer att producera en utmatning som ser ut så här:

```Ruby
7
2
9
```

Du kan också använda srand() för att skapa en sekvens av slumpmässiga nummer som är samma varje gång programmet körs. Detta kan vara användbart för att testa och felsöka din kod.

## Djupdykning

Nu när du vet hur du genererar slumpmässiga nummer i Ruby, låt oss titta lite närmare på hur detta faktiskt fungerar. Ruby använder en algoritm som kallas Mersenne Twister för att producera slumpmässiga tal, och srand() sätter ett så kallat "seed value" för denna algoritm.

Seed-värdet är viktigt eftersom det är det som bestämmer vilken sekvens av slumpmässiga nummer som kommer att produceras. Om du inte sätter ett seed-värde eller använder samma seed-värde varje gång, kommer du att få olika sekvenser av slumpmässiga nummer.

## Se även

- Mersenne Twister: https://en.wikipedia.org/wiki/Mersenne_Twister
- Ruby's built-in methods: https://ruby-doc.org/core-2.7.1/Random.html
- Användbara kodningsprojekt: https://github.com/discourse/random_number_generator