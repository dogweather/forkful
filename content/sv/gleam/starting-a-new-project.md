---
title:                "Gleam: Att påbörja ett nytt projekt."
programming_language: "Gleam"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Varför

Att starta ett nytt projekt kan verka överväldigande, men det finns många fördelar med att använda Gleam-programmering för ditt nästa projekt. Gleam är ett funktionellt programspråk som är utformat för att vara lättläst och lättförståeligt, vilket gör det till ett bra val för både nya och erfarna utvecklare. Dessutom finns det ett starkt community och stöd för Gleam, vilket gör det till ett pålitligt val för dina projektbehov.

## Hur man gör

För att komma igång med Gleam-programmering behöver du först installera Gleam-compiler och Gleam-paketet från deras officiella hemsida. När du har gjort det kan du skapa ditt första projekt i Gleam genom att följa dessa steg:

```
Gleam nytt projektnamn
cd projektnamn
```

Nu är du redo att börja programmera i Gleam! Här är ett exempel på en enkel funktion som tar emot två heltal och returnerar deras produkt:

```
fn multiplicera(x, y) {
  x * y
}

main() {
  let resultat = multiplicera(5, 10)
  IO.println(resultat)
}
```

Kör sedan ditt program genom att köra följande kommando:

```
Gleam kör
```

Output: 50

Ovanstående kod skapar en funktion som heter "multiplicera" och kallas sedan i huvudfunktionen "main". Funktionen tar emot två parametrar och använder sedan operatorn "*" för att multiplicera dem och returnera resultatet. I "main" funktionen används detta resultat sedan för att skriva ut det till konsolen med hjälp av IO.println.

Du kan testa och utveckla ditt Gleam-projekt ytterligare genom att använda olika typer av datastrukturer, loopar, jämförelseoperatorer och mycket mer. Det finns en mängd resurser online för att lära sig mer om Gleam och för att få inspiration till dina projekt.

## Djupdykning

För att få ut mesta möjliga av ditt Gleam-projekt, är det viktigt att förstå de grundläggande delarna i programspråket. Gleam är ett starkt typat språk, vilket innebär att alla variabler måste deklareras med en specifik typ och inte kan ändras senare. Detta leder till mer stabila och säkra program.

Gleam använder också funktionell programmering, vilket innebär att funktioner är förstaklasss medborgare och därmed kan användas som argument till andra funktioner eller returneras från funktioner. Detta gör det möjligt att skriva kompakt och elegant kod.

En annan viktig del av Gleam är moduler, som låter dig organisera ditt projekt i åtskilda filer och moduler, vilket gör det lättare att hantera och underhålla större projekt.

## Se också

- Gleams officiella hemsida: [https://gleam.run/](https://gleam.run/)
- Gleam-paket: [https://lib.rs/crates/gleam](https://lib.rs/crates/gleam)
- Gleam-communityn: [https://github.com/gleam-lang/gleam/discussions](https://github.com/gleam-lang/gleam/discussions)