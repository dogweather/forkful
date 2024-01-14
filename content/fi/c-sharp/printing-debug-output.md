---
title:                "C#: Virheenjäljitystulosteen tulostaminen"
programming_language: "C#"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

### Miksi?

Printtaaminen debug-tulostetta on tärkeä osa ohjelmointia, koska se auttaa kehittäjiä löytämään ja korjaamaan virheitä ja ongelmia koodissa. Se on myös erinomainen tapa oppia ja ymmärtää koodin toimintaa.

### Miten?

Koodin debuggaaminen voi olla haastavaa ja aikaa vievää, mutta printtaaminen debug-tulostetta voi tehdä siitä helpompaa. Käytä ```Console.WriteLine()``` -komennolla tulostaaksesi muuttujien arvot ja viestit koodissa.

```C#
int x = 5;
string message = "Hei maailma!";
Console.WriteLine("x:n arvo on: " + x); // Tulostaa "x:n arvo on: 5"
Console.WriteLine(message); // Tulostaa "Hei maailma!"
```

Et voi vain tulostaa muuttujien arvoja, vaan voit myös käyttää ehtolauseita ja silmukoita tulostaaksesi tietoa. Tämä auttaa sinua ymmärtämään tarkemmin koodin toimintaa ja löytämään mahdolliset ongelmat.

```C#
for (int i = 0; i < 10; i++)
{
    Console.WriteLine("i:n arvo on: " + i);
}
// Tulostaa luvut 0-9
```

### Syvällinen sukellus

On myös hyödyllistä tietää, että voit käyttää muita vastaavia komentoja, kuten ```Debug.WriteLine()``` ja ```Trace.WriteLine()```, riippuen tarpeistasi. Myös C# 6.0 ja uudemmat versiot tarjoavat uusia tapoja printata debug-tulosteita, kuten käyttämällä {n} -muotoiluja ja C# 7.0 tarjoaa mahdollisuuden käyttää inline-muotoiluja.

Ymmärtääksesi tarkemmin, kuinka printata debug-tulosteita ja miten voit käyttää niitä hyödyksesi, lue Microsoftin opas aiheesta. Se tarjoaa kattavaa tietoa ja esimerkkejä, joita voit käyttää oman koodisi debuggaamiseen.

### Katso myös

- Microsoft - Debuggaus dokumentaatio: https://docs.microsoft.com/fi-fi/dotnet/standard/using-threads/debugging
- C# - Debuggaus opas: https://docs.microsoft.com/fi-fi/dotnet/csharp/programming-guide/debugging/