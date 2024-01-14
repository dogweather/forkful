---
title:                "Python: Kontrollera om en mapp finns"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför
Att kontrollera om en mapp existerar är en viktig del av att säkerställa att ditt Python-program körs smidigt. Det är även ett viktigt steg i felsökningen om ditt program inte fungerar som det ska.

## Så här gör du
För att kontrollera om en mapp existerar i Python, använder du en funktion som heter `path.isdir()` från modulen `os.path`. Du kan också importera hela `os` biblioteket med `import os` för att undvika dubbla imports  om andra funktioner från `os` redan är importerade.

```python
import os

# Kontrollera om mappen "Documents" existerar i ditt nuvarande arbetsutrymme
if os.path.isdir("Documents"):
    print("Mappen finns redan")
else:
    print("Mappen finns inte")
```

Om mappen "Documents" existerar, kommer programmet att skriva ut "Mappen finns redan". Om den inte existerar, kommer det istället att skriva ut "Mappen finns inte".

Du kan också använda funktionen `path.exists()` för att kontrollera om en fil eller en mapp existerar, istället för att använda `path.isdir()` som endast kontrollerar för mappar.

```python
import os

# Kontrollera om filen "readme.txt" existerar 
if os.path.exists("readme.txt"):
    print("Filen finns")
else:
    print("Filen finns inte")
```

## Djupdykning
När du använder funktionerna `path.isdir()` och `path.exists()` måste du se till att du har skrivit in den korrekta sökvägen till filen eller mappen du vill kontrollera. Om du inte anger hela sökvägen, kommer Python att leta efter filen eller mappen i ditt nuvarande arbetsutrymme.

Du kan också använda `os.getcwd()` för att få det nuvarande arbetsutrymmet och sedan använda `os.chdir()` för att ändra arbetsutrymmet om det behövs.

```python
import os

print(os.getcwd()) # Skriver ut det nuvarande arbetsutrymmet

os.chdir("Documents") # Ändrar arbetsutrymmet till mappen "Documents"

print(os.getcwd()) # Skriver ut det uppdaterade arbetsutrymmet
```

## Se även
- [Python dokumentation om `os.path` modulen](https://docs.python.org/3/library/os.path.html)
- [Guide till filhantering i Python](https://realpython.com/working-with-files-in-python/)
- [Mer om arbetsutrymmen i Python](https://www.learnpython.org/en/Variables_and_Types#:~:text=Working%20Directory%20Variables%20in%20Python&text=The%20current%20working%20directory%20is,8%3E%3E%3E%20walking%20%C2%BB%3E%3E)