---
title:                "Satunnaisten numeroiden luominen"
html_title:           "PowerShell: Satunnaisten numeroiden luominen"
simple_title:         "Satunnaisten numeroiden luominen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Yksityiskohtaisesti: Satunnaisten Lukujen Generointi PowerShellilla 

## Mitä & Miksi?

Satunnaislukujen generointi on toiminto, joka tuottaa ennalta-arvaamattomia numeroita. Ohjelmoijat käyttävät tätä esim. testidatan luonnissa, simuloinneissa ja peliohjelmoinnissa.

## Miten toimitaan:

PowerShellissa satunnaislukujen generointi on erittäin helppoa. Esimerkiksi, jos haluat tuottaa satunnaisen kokonaisluvun väliltä 0-100, kirjoitat seuraavasti:

```PowerShell
$luku = Get-Random -Minimum 0 -Maximum 100
$luku
```

Käytämme `Get-Random` -cmdletiä tässä. Se voi ottaa `Minimum`- ja `Maximum`-parametrit arvojen rajoittamiseksi. Ajo tämä koodi tuottaa satunnaisen luvun väliltä 0-100.

## Syväsukellus:

PowerShellissa on käytetty satunnaislukujen algoritmeja sen varhaisista versioista lähtien. Sen `Get-Random`-cmdlet käyttää .NET:n System.Random-luokkaa, joka on mukana C#-kielestä lähtien.

Satunnaisten lukujen generoimiseksi on olemassa muitakin tapoja, kuten Mersenne Twister -algoritmi tai kryptografisesti turvalliset algoritmit, mutta ne vaativat enemmän koodia.

Sisäisesti, `Get-Random` luo uuden System.Random-instanssin joka kerta kun sitä kutsutaan, varmistaen että luvut ovat todellakin satunnaisia.

## Katso Myös:

Seuraavat lähteet ovat hyödyllisiä sekä aloittelijoille että kokeneille ohjelmoijille:

- Microsoft PowerShell-dokumentaatio: [Get-Random](https://docs.microsoft.com/fi-fi/powershell/module/microsoft.powershell.utility/get-random)
- .NET-dokumentaatio: [System.Random](https://docs.microsoft.com/fi-fi/dotnet/api/system.random) 
- PowerShellin [Get-Random cmdletin](https://learn-powershell.net/2013/07/02/use-get-random-in-powershell-to-get-a-random-number/) käyttö
- Satunnaisten lukujen generointia käsittelevä [Wikipedia-artikkeli](https://fi.wikipedia.org/wiki/Satunnaisluku) (englanniksi)