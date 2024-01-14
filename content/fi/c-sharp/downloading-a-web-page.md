---
title:                "C#: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi
Web-sivujen lataaminen on tärkeä osa monien verkkosovellusten toimintaa, kuten verkkoselaajien, kuvakaappauksen tekijöiden ja tiedostojen latausohjelmien. Se mahdollistaa tietojen hakemisen internetistä ja niiden käyttämisen sovelluksissamme.

## Miten
Tässä esimerkissä käytämme C#-ohjelmointikieltä ja sen .NET Frameworkin WebRequest-luokkaa, joka tarjoaa helpon tavan ladata verkkosivuja. Aloita luomalla uusi konsoli-sovellus Visual Studiolla ja lisää seuraava koodi `Main`-funktioon:

```C#
using System;
using System.Net;

namespace WebPageDownloader
{
    class Program
    {
        static void Main(string[] args)
        {
            // Luodaan uusi WebRequest-objekti ja annetaan URL-osoite parametrina
            WebRequest request = WebRequest.Create("https://www.example.com");

            // Suoritetaan pyyntö ja tallennetaan vastaus-objekti
            WebResponse response = request.GetResponse();

            // Haetaan vastauksen sisältö ja tallennetaan se lukijaan
            using (System.IO.StreamReader reader = new System.IO.StreamReader(response.GetResponseStream()))
            {
                string text = reader.ReadToEnd();
                Console.WriteLine(text);
            }
        }
    }
}
```

Ohjelmassa luodaan ensin uusi WebRequest-objekti, jolle annetaan halutun verkkosivun URL-osoite parametrina. Tämän jälkeen suoritetaan pyyntö ja tallennetaan vastaus-objekti, joka sisältää verkkosivun tiedot. Lopuksi haetaan vastauksen sisältö ja tulostetaan se konsolille.

Ohjelman suoritettaessa se tulostaa halutun verkkosivun HTML-koodin. Voit myös muokata koodia ja tallentaa vastauksen vaikkapa tiedostoon tai käyttää hankitun datan muilla tavoin sovelluksessasi.

## Syventävä tarkastelu
Kyseessä oli yksinkertainen esimerkki web-sivun lataamisesta käyttäen C#-kieltä ja .NET Frameworkia. WebRequest-luokka tarjoaa myös muita hyödyllisiä ominaisuuksia, kuten mahdollisuuden lähettää HTTP-pyyntöjä ja vastaanottaa HTML-tiedon sijaan myös muita tiedostomuotoja. Voit tutustua tarkemmin luokan dokumentaatioon Microsoftin sivuilta.

## Katso myös
- [Microsoftin WebRequest-luokan dokumentaatio](https://docs.microsoft.com/en-us/dotnet/api/system.net.webrequest?view=netcore-3.1)
- [C#-opas: Web-sivujen lataaminen](https://docs.microsoft.com/en-us/dotnet/csharp/tutorials/intro-to-csharp/introduction-to-the-csharp-language-and-the-net-framework#download-a-web-page)