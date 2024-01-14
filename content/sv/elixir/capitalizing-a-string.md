---
title:    "Elixir: Kapitalisering av en sträng"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Varför

Att forma en sträng med stora bokstäver kan vara en vanlig uppgift i många programmeringsprojekt, särskilt när du hanterar textdata. Det kan vara användbart för att skapa enhetliga strängar, filtrera och sortera data eller bara för att förbättra läsbarheten av utdata. I denna bloggpost ska vi titta på hur du kan göra detta enkelt i Elixir.

## Så här gör du

För att börja, låt oss definiera en variabel med en sträng som vi vill kapitalisera:

```Elixir
sträng = "detta är en sträng som ska kapitaliseras"
```

För enkel kapitalisering finns det en inbyggd funktion i Elixir som heter `String.capitalize/1` som kan användas på följande sätt:

```Elixir
String.capitalize(sträng)
```

Detta skulle ge resultatet "Detta är en sträng som ska kapitaliseras". Observera att denna funktion bara kapitaliserar första bokstaven i varje ord och ignorera alla andra bokstäver. Om vi vill att alla bokstäver ska vara stora kan vi använda funktionen `String.upcase/1`:

```Elixir
String.upcase(sträng)
```

Detta skulle ge utdatan "DETTA ÄR EN STRÄNG SOM SKA KAPITALISERAS". Om vi bara vill att den första bokstaven ska vara stor och resten små, kan vi använda funktionen `String.normalize/1`:

```Elixir
String.capitalize(sträng)
```

Utmatningen skulle vara "Detta är en sträng som ska kapitaliseras". Det finns också möjligheter att kapitalisera en sträng baserat på andra kriterier, som till exempel omvänd ordning, första och sista bokstaven i en mening etc. Det finns också specifika funktioner för Unicode-strängar som kan vara användbara för olika språk.

## Djupdykning

Elixir har olika typer av strängar, som binära, som har fördelar som är lämpliga för vissa användningsområden. Det finns också många inbyggda funktioner som du kan använda för att manipulera strängar på olika sätt. Vissa av dem har redan nämnts ovan, men det finns också funktioner som `String.split/2` för att dela upp en sträng baserat på ett visst tecken eller `String.replace/4` för att ersätta en del av en sträng med en annan. Det är också möjligt att använda reguljära uttryck när du bearbetar strängar i Elixir.

Utforska gärna Elixirs dokumentation för att hitta fler användbara funktioner för att hantera strängar.

## Se även

- Elixir Dokumentation om [string manipulation](https://elixir-lang.org/getting-started/strings.html)
- [Elixir School](https://elixirschool.com/) - en interaktiv plattform för att lära sig Elixir
- [Elixir Forum](https://elixirforum.com/) - en gemenskapsdriven plats för att ställa frågor och få hjälp med Elixir-programmering