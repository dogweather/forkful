---
title:                "C: Utskrift av feilrettingsutdata"
simple_title:         "Utskrift av feilrettingsutdata"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Feilsøking er en essensiell del av programmering, og å printe ut debug-informasjon kan være en nyttig måte å finne feil i koden din. Det kan også hjelpe deg med å forstå hva som skjer under kjøringen av programmet ditt.

## Slik gjør du det

Når du skal printe debug-informasjon i C, kan du bruke funksjonen `printf()`. Denne funksjonen tar imot en eller flere variabler og printer dem ut på skjermen. La oss se på et enkelt eksempel:

```C
int num = 10;
printf("Dette er verdien til num: %d", num);
```

I dette eksemplet definerer vi en variabel `num` med verdien 10. Deretter bruker vi `printf()` for å printe ut verdien til `num` på skjermen. Merk at `%d` i teksten betyr at vi skal printe ut en heltallsvariabel. Outputen vil være:

`Dette er verdien til num: 10`

Du kan også bruke flere variabler i samme `printf()`-funksjon, og du kan også bruke forskjellige formateringskoder for å printe ut forskjellige typer variabler.

```C
int num1 = 5;
float num2 = 3.14;
printf("Tallet er %d og pi er %f", num1, num2);
```

Her bruker vi `%d` for å printe ut tallet `num1` og `%f` for å printe ut verdien til `num2`, som er et flyttall. Outputen vil være:

`Tallet er 5 og pi er 3.14`

Dette er bare noen få eksempler på hvordan du kan bruke `printf()`-funksjonen for å printe debug-informasjon i C. Ved å eksperimentere med forskjellige formateringskoder og variabler, kan du få en dypere forståelse av hvordan denne funksjonen fungerer.

## Dypdykk

Det finnes flere spesielle formateringskoder du kan bruke i `printf()`-funksjonen, som for eksempel `%c` for å printe ut en tegnvariabel, `%s` for å printe ut en streng, og `%p` for å printe ut en peker. Du kan også bruke spesielle kontrollsekvenser som `\n` for å lage en ny linje i outputen eller `\t` for å lage et tabulatoravstand.

Det er også mulig å printe ut verdien til en variabel på en bestemt bredde ved å bruke følgende syntaks: `%Nnumber`, der N er antall plasser du vil reservere for variabelen. For eksempel:

```C
int num = 5;
printf("%4d", num);
```

Her vil verdien til `num` bli printet ut på 4 plasser, uavhengig av hvor stor tallet faktisk er. Hvis tallet er mindre enn 4 siffer, vil det bli lagt til ekstra mellomrom på plassen før outputen.

## Se også

* [C - printf() function](https://www.tutorialspoint.com/c_standard_library/c_function_printf.htm)
* [Debugging in C: A Simple Guide](https://www.samrayner.com/posts/debugging-in-c-a-simple-guide/)
* [Advanced Debugging Techniques in C](https://www.linuxjournal.com/article/6930)