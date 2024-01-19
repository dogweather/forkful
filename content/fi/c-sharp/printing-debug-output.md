---
title:                "Debug-tulosteen tulostaminen"
html_title:           "Bash: Debug-tulosteen tulostaminen"
simple_title:         "Debug-tulosteen tulostaminen"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

### Mitä & Miksi?

Printtauksen debug tuloste on menetelmä, jota ohjelmoijat käyttävät nähdäkseen, mitä ohjelmassa tapahtuu. Se on erityisen hyödyllinen vianmäärityksessä ja testauksessa.

### Näin se toimii:

C#-koodin kirjoittamiseksi, meidän täytyy importata `System.Diagnostics` kirjasto. Käytämme `Debug.WriteLine` metodia tulostamaan viestin, kun ohjelma ajetaan debug-tilassa.

```C#
using System.Diagnostics;

class Program
{
    static void Main()
    {
        Debug.WriteLine("Tämä on debug-viesti");
    }
}
```

Kun yllä oleva ohjelma suoritetaan, "Tämä on debug-viesti" tulostetaan debug-ikkunaan. 

### Sukellus syvemmälle:

Historiallisesti ajatellen, printtauksen debug tulostus on ollut tärkeä työkalu ohjelmoijille. Sen avulla he voivat ymmärtää monimutkaisia ohjelmia tai löytää odottamattomia virheitä.

Printtauksen debug-tulosteiden ohella on olemassa monia muita vianmääritystyökaluja, kuten interaktiiviset debuggerit tai lokitiedostot.

C#-käytössä Debug-luokka käyttää TraceListener-objekteja, jotka määrittävät, minne debug-viestit ohjataan. Oletusarvoisesti Visual Studio ohjaa viestit Output-ikkunaan, kun ohjelma suoritetaan debug-tilassa.

### Katso myös:

1. [Microsoftin dokumentaatio Debug-luokasta](https://docs.microsoft.com/fi-fi/dotnet/api/system.diagnostics.debug)
2. [Microsoftin ohjeet vianmääritykseen C#-ohjelmissa](https://docs.microsoft.com/fi-fi/visualstudio/debugger/)
3. [Stack Overflow C# debug output -kysymykset](https://stackoverflow.com/questions/tagged/debugoutput?tab=Votes)