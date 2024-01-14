---
title:                "Python: Skapa en tillfällig fil"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Att skapa en temporär fil kan vara användbart när du behöver skapa en fil som en del av ditt program men inte vill behålla den permanent. Det kan också vara användbart när du behöver dela data mellan olika delar av ditt program eller när du vill undvika att ha flera kopior av samma fil på ditt system.

## Så här gör du

Det finns flera sätt du kan skapa en temporär fil på i Python, men ett vanligt sätt är att använda modulen "tempfile". Här är ett exempel på hur du kan göra det:

```Python
import tempfile

# Skapa en temporär fil och returnera dess filobjekt
temp_file = tempfile.TemporaryFile()

# Skriv data till filen
temp_file.write("Hej, världen!".encode())

# Öppna filen för läsning
temp_file.seek(0)
print(temp_file.read().decode())

# Stäng filen och ta bort den
temp_file.close()
```

Detta kodexempel kommer att skapa en temporär fil och skriva "Hej, världen!" till den. Sedan läser den innehållet från filen och skriver ut det. Till sist stängs filen och raderas automatiskt av systemet.

Du kan också specificera en mapp där den temporära filen ska sparas, genom att ange "dir" parameter i "TemporaryFile"-funktionen. Om inget "prefix" anges kommer filen att få ett slumpmässigt genererat namn, men du kan också ange ett prefix för filnamnet genom att ange "prefix" parameter.

```Python
# Skapa en temporär fil i mappen "Temp" med prefix "data_"
temp_file = tempfile.TemporaryFile(dir="Temp", prefix="data_")
```

## Djupdykning

Temporära filer skapas vanligtvis för att användas som en slags buffert eller temporär lagringsplats för data. Exempel på sådana användningsområden kan vara att skapa en temporär fil för att spara indata från en användare, skapa en temporär fil för att lagra en kopia av en befintlig fil eller använda en temporär fil för att lagra data som ska skickas till en annan del av programmet.

När du använder "TemporaryFile"-funktionen från "tempfile"-modulen så är filen som skapas automatiskt raderbar, vilket innebär att den kommer att tas bort när filobjektet stängs. Om du vill behålla filen permanent behöver du använda "NamedTemporaryFile"-funktionen istället. Den fungerar på samma sätt som "TemporaryFile"-funktionen men filen som skapas kommer att behållas även när filobjektet stängs, och kan sedan raderas manuellt om så önskas.

## Se även

- [Dokumentation för "tempfile"-modulen](https://docs.python.org/3/library/tempfile.html)
- [En guide för att skriva temporära filer i Python](https://www.pythontutorial.net/python-basics/python-create-temporary-file/)