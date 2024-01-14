---
title:    "C: Att skriva en textfil"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att skriva en textfil är ett viktigt koncept inom programmering och kan användas för många olika ändamål. Det kan vara till nytta för att spara data, skapa loggar eller för att kommunicera med andra program. I denna bloggpost kommer vi att utforska hur man skriver en textfil i C-programmering.

## Hur man gör det

Att skriva en textfil i C är en relativt enkel process. Det första steget är att deklarera en pekare till en fil. Det kan se ut så här:

```C
FILE *fptr;
```

Sedan måste vi öppna filen i önskat läge, som i detta fall är "w" för att skriva till en fil.

```C
fptr = fopen("mitt_texter.txt", "w");
```

När filen är öppen och redo att skrivas till kan vi använda funktionen "fprintf" för att skriva texten till filen. Den tar in två parametrar - pekaren till filen och den text som ska skrivas. Se följande exempel:

```C
fprintf(fptr, "Hej, det här är en text som skrivs till filen\n");
```

När vi är klara med att skriva till filen måste vi stänga den för att säkerställa att allt sparas korrekt.

```C
fclose(fptr);
```

Nu har vi skapat en textfil med den text som vi ville skriva till den.

## Djupdykning

När vi skriver en textfil kan vi även använda variabler eller andra värden i vårt printf-statement. Till exempel:

```C
int nummer = 42;
fprintf(fptr, "Detta är talet %d som är mitt favoritnummer\n", nummer);
```

Du kan även använda loopar för att skriva till en fil. Till exempel om vi vill skriva alla jämna tal från 1 till 10 kan vi göra så här:

```C
for(int i = 2; i <= 10; i+=2) {
    fprintf(fptr, "%d\n", i);
}
```

Detta kommer att skriva följande in i vår fil:

```
2
4
6
8
10
```

## Se även

- [C Standard Library - FILE](https://www.tutorialspoint.com/c_standard_library/c_function_fclose.htm)