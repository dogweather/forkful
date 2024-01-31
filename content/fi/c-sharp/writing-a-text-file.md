---
title:                "Tekstitiedoston kirjoittaminen"
date:                  2024-01-19
html_title:           "Arduino: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Tekstitiedoston kirjoittaminen tarkoittaa merkkijonojen tallentamista tiedostoon. Ohjelmoijat tekevät tätä datan säilömiseksi, siirtämiseksi tai logien luomiseksi.

## How to:
Käytä `System.IO` nimiavaruutta.

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string tiedostonimi = "esimerkki.txt";
        string teksti = "Hei Suomi!";

        File.WriteAllText(tiedostonimi, teksti);

        Console.WriteLine("Tiedostoon kirjoitettu.");
    }
}
```

Sample output:
```
Tiedostoon kirjoitettu.
```

Lisäys:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string tiedostonimi = "esimerkki.txt";
        string lisattavaTeksti = "Moikka maailma!";

        using (StreamWriter kirjoittaja = File.AppendText(tiedostonimi))
        {
            kirjoittaja.WriteLine(lisattavaTeksti);
        }

        Console.WriteLine("Teksti lisätty tiedostoon.");
    }
}
```

Sample output:
```
Teksti lisätty tiedostoon.
```

## Deep Dive
Tekstitiedostojen kirjoittaminen on ollut osa ohjelmointia alusta asti. `StreamWriter` ja `File`-luokat ovat nykyajan työkaluja, muttet rajattu vain niihin. Vaihtoehtoisesti käytössä olisivat `FileStream`, `MemoryStream`, tai jopa vanhempi `System.IO.StreamWriter`.

Tiedoston kirjoittamisessa pitää huomata kaksi tyyliä: ylikirjoittaminen ja lisääminen. Ylikirjoittaminen (`WriteAllText`) alkaa aina tyhjästä, kun taas lisääminen (`AppendText`) säilyttää olemassaolevan sisällön.

## See Also
- Microsoft Docs, File.WriteAllText: https://docs.microsoft.com/en-us/dotnet/api/system.io.file.writealltext
- Microsoft Docs, StreamWriter: https://docs.microsoft.com/en-us/dotnet/api/system.io.streamwriter
- Stack Overflow haku "C# write to file": https://stackoverflow.com/search?q=c%23+write+to+file
