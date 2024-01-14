---
title:    "Gleam: Att starta ett nytt projekt"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

##Varför
Om du är en programmerare som vill utveckla en snabb, funktionell och tillförlitlig webbapplikation, då är Gleam det perfekta språket för dig. Med sin moderna syntax och starka statiska typsystem, är Gleam idealiskt för att bygga skalbara system som kan hantera komplexa problem. Så om du letar efter ett kraftfullt och effektivt programmeringsspråk, då är det dags att börja utforska Gleam för dina projekt.

##Hur man gör
För att komma igång med Gleam, behöver du först installera Elixir, som är en förutsättning för att köra Gleam. Sedan kan du installera Gleam genom att köra kommandot ``gem install gleam`` i din terminal. Efter installationen kan du skapa ett nytt projekt genom att köra kommandot ``gleam new project_name``. Detta kommer att skapa en grundläggande struktur för ditt projekt och en fil med namnet ``main.gleam``, som är platsen där du kan börja koda.

När du öppnar ``main.gleam``-filen kommer du att se något som detta:

```gleam
//// main.gleam ////
// Detta är en kommentar
pub fn main() {
  let name = "Världen"
  let message = greeter(name)
  debug_log(message)
}

fn greeter(name) {
  "Hej" ++ name ++ "!"
}
```
Du kan se att det finns en ``main``-funktion som kör en ``greeter``-funktion som hälsar på en person. Du kan ändra värdet på ``name``-variabeln för att prova olika hälsningar. Efter att ha ändrat koden, kan du köra den genom att använda kommandot ``gleam run main.gleam`` i din terminal. Då kommer du att se utmatningen från din ändrade kod.

##Fördjupning
Nu när du har bekantat dig med Gleam och dess grundläggande syntax, kan du utforska mer av vad språket har att erbjuda. Gleam erbjuder stöd för funktionell programmering genom mönstermatchning, immutabla värden och högre ordningens funktioner. Det är också möjligt att integrera Gleam med andra språk som till exempel Elixir eller Erlang, som ger dig tillgång till deras kraftfulla bibliotek och verktyg.

För att lära dig mer om Gleam och dess möjligheter, kan du besöka deras officiella hemsida och dokumentation:

- https://gleam.run/
- https://gleam.run/getting-started/
- https://gleam.run/learn/

##Se även
- [Elixir](https://elixir-lang.org/)
- [Erlang](https://www.erlang.org/)
- [Funktionell programmering](https://www.ibm.com/cloud/learn/functional-programming)