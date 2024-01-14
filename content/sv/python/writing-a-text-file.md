---
title:    "Python: Skriva en textfil"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att kunna skriva till textfiler är en grundläggande förmåga inom programmering. Det är en effektiv metod för att lagra, organisera och bearbeta data. Genom att lära sig hur man skriver till textfiler kan man också enkelt skapa rapporter, spara användarinformation och mycket mer.

## Hur man gör det

Att skriva till en textfil med Python är en relativt enkel process. Först måste vi öppna en fil med hjälp av inbyggda funktionen `open()`. Detta tar två argument, filnamnet och vilket läge vi vill öppna filen i. Till exempel kan vi använda läget "w" för att öppna filen för skrivning.

```Python
# Öppna filen "mitt_textdokument.txt" för skrivning
textfil = open("mitt_textdokument.txt", "w")
```

Nästa steg är att skriva vårt innehåll till filen. Detta görs med hjälp av `.write()` metoden. Notera att vi måste avsluta varje rad med ett radslutstecken, som `"\n"` för att få en ny rad i filen.

```Python
# Skriv lite text till vår textfil
textfil.write("Hej, detta är mitt första textdokument!\n")
textfil.write("Hoppas du hittar det lätt att följa med.\n")
```

Slutligen måste vi stänga filen för att säkerställa att all information sparats. Detta görs genom att använda `.close()` metoden.

```Python
# Stäng filen
textfil.close()
```

Efter detta kan vi öppna filen och se vårt skrivna innehåll i en textredigerare.

## Djupdykning

När vi skriver till en fil med hjälp av Python, konverteras allt till en sträng. Detta betyder att om vi vill skriva andra datatyper, som en lista eller en dictionary, måste den först konverteras till en sträng.

```Python
# Skriv en lista till vår textfil (observera användningen av str() för att konvertera till sträng)
min_list = ["fotboll", "hockey", "basket"]
textfil.write(str(min_list))
```

Det finns många olika metoder som kan användas när du skriver till textfiler, till exempel `.writelines()` för att skriva flera rader på en gång eller `.seek()` för att ändra positionen i filen.

En annan användbar funktion är `with`, som automatiskt stänger filen åt oss när vi har skrivit klart. Detta är särskilt användbart om vi arbetar med större filer eller behöver skriva till flera filer samtidigt.

```Python
# Använda "with" för automatisk stängning av filen
with open("mitt_andra_dokument.txt", "w") as andra_fil:
    andra_fil.write("Det här är mitt andra textdokument!")
```

## Se även

* [Python officiella dokumentationen om filhantering](https://docs.python.org/sv/3/tutorial/inputoutput.html#reading-and-writing-files)
* [En grundläggande guide till filhantering i Python](https://www.pythonforbeginners.com/files/reading-and-writing-files-in-python)
* [En tutorial om hur man skriver till CSV-filer i Python](https://realpython.com/python-csv/)

Tack för att ni läst denna guide om att skriva textfiler i Python! Genom att behärska denna färdighet kan du öppna upp en hel värld av möjligheter inom programmering. Lycka till!