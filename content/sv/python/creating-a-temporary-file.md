---
title:    "Python: Skapa en tillfällig fil"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Varför

I programmering, finns det ofta ett behov av att skapa en temporär fil som endast behövs under en kort period av tid. Detta kan vara för att spara tillfälliga data eller för att utföra temporära beräkningar. Att använda temporära filer kan hjälpa till att hålla koden renare och minska risken för konflikter med andra filer. 

## Så här gör du

I Python kan du enkelt skapa en temporär fil med hjälp av modulen "tempfile". Här är ett exempel på hur du kan skapa en temporär fil och sedan skriva till den:

```python
import tempfile

# Skapa en temporär fil med namnet "temp_file"
temp_file = tempfile.NamedTemporaryFile()

# Skriv till filen
temp_file.write(b"Det här är innehållet i min temporära fil.")

# Stäng filen
temp_file.close()
```

Om du vill skapa en specifik temporär filtyp, till exempel en CSV-fil, kan du göra det genom att ange rätt suffix när du skapar den temporära filen:

```python
import tempfile

# Skapa en temporär CSV-fil med namnet "temp_csv_file"
temp_csv_file = tempfile.NamedTemporaryFile(suffix=".csv")
```

Om du vill läsa från en befintlig temporär fil kan du använda funktionen "open" och sedan stänga filen när du är klar:

```python
import tempfile

# Skapa en temporär fil med namnet "temp_file"
temp_file = tempfile.NamedTemporaryFile()

# Läsa från filen
with open(temp_file.name) as f:
    content = f.read()

# Stäng filen
temp_file.close()
```

## Djupdykning

När du skapar en temporär fil med hjälp av "tempfile" modulen, skapas filen på en plattformsspecifik plats. Detta innebär att du inte behöver oroa dig för att ge filen ett unikt namn då det redan kommer att vara unikt baserat på platsen där det skapas.

Du kan också använda "tempfile" modulen för att skapa temporära mappar genom att använda funktionen "mkdtemp" istället för "NamedTemporaryFile". Detta kan vara användbart om du behöver lagra flera temporära filer eller om du behöver en temporär plats att arbeta med.

## Se även

- [Dokumentation för Python Tempfile-modulen](https://docs.python.org/3/library/tempfile.html)
- [Python grundläggande tutorial](https://www.pythonforbeginners.com/basics/python-tutorial-temporary-file)
- [YouTube tutorial på att skapa temporära filer i Python](https://www.youtube.com/watch?v=6dSBskC3IeM)