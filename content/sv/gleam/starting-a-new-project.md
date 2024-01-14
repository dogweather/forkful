---
title:                "Gleam: Att påbörja ett nytt projekt"
simple_title:         "Att påbörja ett nytt projekt"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Varför

Att starta ett nytt projekt kan vara en spännande och utmanande uppgift för alla som är intresserade av programmering. Det ger dig möjligheten att skapa något unikt och utveckla dina kunskaper inom Gleam-programmering.

## Så här gör du

Att starta ett nytt projekt i Gleam kan vara enkelt och roligt. Du behöver bara följa några enkla steg:

```Gleam
git init mittprojekt
cd mittprojekt
gleam new app hello_world
```

Det här kommer att skapa en ny app med namnet "hello_world" i ditt projekt. Du kan sedan öppna den i ditt favoritredigeringsprogram och börja koda!

För att bygga och köra din app kan du använda följande kommandon:

```Gleam
gleam build
gleam run
```

Outputen av din app kommer att visas i terminalen. Grattis, du har nu startat ditt första projekt i Gleam!

## Djupdykning

För att verkligen ta ditt nya projekt till nästa nivå, finns det några saker du kan göra:

1. Lägg till beroenden: Med Gleams pakethanterare kan du enkelt lägga till beroenden till ditt projekt och använda dem i din kod.
2. Skriv tester: Testning är viktigt för att säkerställa att din kod fungerar som den ska. Genom att skriva tester kan du upptäcka och fixa buggar innan de hamnar i produktion.
3. Använd moduler: Gleam tillåter dig att organisera din kod i moduler för att hålla den mer strukturerad och lättare att underhålla.

Se till att utforska Gleams dokumentation och community för mer information om hur du kan utveckla ditt projekt.

## Se även

- [Gleam's officiella dokumentation](https://gleam.run)
- [Gleam's GitHub repository](https://github.com/gleam-lang/gleam)