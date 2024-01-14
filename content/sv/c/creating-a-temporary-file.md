---
title:    "C: Skapa en temporär fil"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

I C-programmering är det ofta nödvändigt att skapa tillfälliga filer för att hantera temporära data. Dessa filer kan användas för att temporärt lagra information under körning av programmet eller för att kommunicera med andra program. Att lära sig att skapa tillfälliga filer är en viktig färdighet för en C-programmerare.

## Hur man gör det

För att skapa en tillfällig fil i C, behöver vi använda en funktion som heter `tmpfile()` som är definierad i stdio.h biblioteket. Denna funktion returnerar en FILE-pekare till den temporära filen. Här är ett enkelt exempel på hur man använder `tmpfile()` för att skapa en tillfällig fil:

```C
#include <stdio.h>

int main()
{
    FILE *fp;
    fp = tmpfile();

    if (fp == NULL)
    {
        printf("Fel vid skapande av tillfällig fil\n");
        return 1;
    }

    printf("En tillfällig fil skapad!\n");
    fprintf(fp, "Det här är innehållet i filen.");

    fclose(fp);

    return 0;
}
```

När programmet körs, kommer `tmpfile()` att skapa en temporär fil i systemets standard temporära katalog. Om funktionen lyckas kommer vi skriva in innehållet i filen och sedan stänga den med hjälp av `fclose()`.

Om vi kör programmet igen, kanske vi märker att en ny tillfällig fil med ett nytt namn är skapad. Detta beror på att varje gång vi kör programmet, kommer en ny tillfällig fil att skapas.

## Djupdykning

När en tillfällig fil skapas, kommer den att bli automatiskt borttagen när programmet avslutas. Men det är också möjligt att ta bort filen manuellt innan programmet avslutas genom att använda `remove()` funktionen. Till exempel:

```C
#include <stdio.h>

int main()
{
    FILE *fp;
    fp = tmpfile();

    if (fp == NULL)
    {
        printf("Fel vid skapande av tillfällig fil\n");
        return 1;
    }

    printf("En tillfällig fil skapad!\n");
    fprintf(fp, "Det här är innehållet i filen.");

    // ta bort filen manuellt
    remove(fp);

    fclose(fp);

    return 0;
}
```

Filen kommer då att tas bort direkt på raden `remove(fp);`.

## Se även

- [C-programmeringspråkets officiella hemsida](https://port70.net/~nsz/c/c11/n1570.html)
- [Stdio.h biblioteket på cppreference.com](https://en.cppreference.com/w/c/io)