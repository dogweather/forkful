---
title:                "C#: Kontrollera om en katalog finns"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

### Varför
Att kontrollera om en katalog existerar är en viktig del av C# programmering, eftersom det låter dig se till att nödvändiga filer och resurser finns tillgängliga innan du fortsätter med din kod. Det kan också hjälpa till att undvika fel och undanröja potentiella hinder innan de uppstår.

### Hur man gör
För att kontrollera om en katalog existerar i C#, använder vi oss av metoden `Directory.Exists()` som ingår i `System.IO`-klassen. Nedan följer ett exempel på hur man kan implementera detta i sin kod:

```C#
using System;
using System.IO;

namespace DirectoryCheck
{
    class Program
    {
        static void Main(string[] args)
        {
            // Skapa en variabel för sökvägen till katalogen vi vill kontrollera
            string path = @"C:\Users\Example\User\Documents";

            // Anropar Directory.Exists() och tilldelar resultatet till en bool-variabel
            bool directoryExists = Directory.Exists(path);

            // Kontrollerar om katalogen existerar eller inte
            if(directoryExists)
            {
                Console.WriteLine("Katalogen finns tillgänglig.");
            } 
            else
            {
                Console.WriteLine("Katalogen finns inte tillgänglig.");
            }

            Console.ReadKey();
        }
    }
}
```

**Resultat:**

```
Katalogen finns tillgänglig.
```

### Djupdykning
För att förstå hur `Directory.Exists()` fungerar är det viktigt att känna till att den returnerar `true` om katalogen finns och `false` om den inte finns. Detta gör det enkelt att implementera en if-sats eller en try-catch-block för att hantera existerande eller icke-existerande kataloger.

Det är också värt att nämna att `Directory.Exists()` inte bara fungerar för lokala kataloger, utan också för nätverkskataloger. Detta gör det möjligt att enkelt kontrollera tillgängligheten av resurser på en server eller i ett delat nätverk.

### Se även
- [Microsoft Docs - Directory.Exists Method](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)
- [C# File and Directory Operations](https://www.c-sharpcorner.com/article/c-sharp-file-and-directory-operations/)
- [Checking Directory Existence in C#](https://www.c-sharpcorner.com/UploadFile/mahesh/checking-directory-existence-in-C-Sharp/)