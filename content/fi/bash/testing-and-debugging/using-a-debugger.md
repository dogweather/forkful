---
date: 2024-01-26 03:47:27.348901-07:00
description: "Debuggerin k\xE4ytt\xE4minen Bashissa tarkoittaa ty\xF6kaluihin turvautumista\
  \ skriptien testaamiseksi ja ongelmien l\xF6yt\xE4miseksi, kuten virheiden nappaamista,\
  \ jotka\u2026"
lastmod: 2024-02-19 22:05:15.639470
model: gpt-4-0125-preview
summary: "Debuggerin k\xE4ytt\xE4minen Bashissa tarkoittaa ty\xF6kaluihin turvautumista\
  \ skriptien testaamiseksi ja ongelmien l\xF6yt\xE4miseksi, kuten virheiden nappaamista,\
  \ jotka\u2026"
title: "Debuggerin k\xE4ytt\xF6"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Debuggerin käyttäminen Bashissa tarkoittaa työkaluihin turvautumista skriptien testaamiseksi ja ongelmien löytämiseksi, kuten virheiden nappaamista, jotka kaatavat koodisi tai hiipivästi saavat sen käyttäytymään väärin. Ohjelmoijat tekevät niin, koska virheiden havaitseminen ennen niiden aiheuttamaa tuhoa live-ympäristössä on paljon fiksumpaa.

## Miten:
Bash ei sisällä sisäänrakennettua debuggeria kuten jotkin muut kielet, mutta voit käyttää sisäänrakennettuja komentoja kuten `set -x` seurataksesi, mitä tapahtuu. Tai, parannuksena, on olemassa `bashdb`, asianmukainen debuggeri koodin läpikäymiseen. Tässä esikatsaus:

```Bash
# Debuggauksen käyttö set -x:n avulla
set -x
echo "Aloitetaan debuggaus"
my_var="Hei, Debuggaus Maailma!"
echo $my_var
set +x

# Käyttäen bashdb:tä
# Asenna bashdb paketinhallintasi avulla, esim., apt, yum, brew.
# Debuggaa skriptiä nimeltä my_script.sh:
bashdb my_script.sh
```

Tuloste, kun ajetaan käyttäen `set -x`:
```Bash
+ echo 'Aloitetaan debuggaus'
Aloitetaan debuggaus
+ my_var='Hei, Debuggaus Maailma!'
+ echo 'Hei, Debuggaus Maailma!'
Hei, Debuggaus Maailma!
+ set +x
```

## Syväsukellus
Historiallisesti Bash-skriptien debuggaus tarkoitti koodisi täyttämistä `echo`-lauseilla. Mutta sitten tuli `set -x`, joka antoi meille kurkistuksen suoritusaikaan manuaalisia tulosteita vaatimatta. Ja niille, jotka kaipaavat enemmän kontrollia, ilmestyi `bashdb`-debuggeri, joka sai inspiraationsa gdb-debuggerista C/C++:lle.

Vaihtoehtojen osalta, `set`-komentojen (`-x`, `-v`, `-e`) lisäksi, muita vaihtoehtoja sisältävät tulosteen ohjaaminen tiedostoon analyysia varten tai ulkoisten työkalujen, kuten ShellCheckin, käyttäminen staattiseen analyysiin.

Toteutuksen kannalta, `set -x` on helppo; se on natiivi Bash-vaihtoehto, joka tulostaa komentoja ja niiden argumentteja niiden suoritushetkellä. `bashdb` puolestaan mahdollistaa koodin läpikäymisen, katkaisupisteiden asettamisen ja lausekkeiden arvioinnin - asioita, jotka antavat sinulle taistelumahdollisuuden hankalampia bugeja vastaan.

## Katso Myös
- Bash Debugger -projekti: http://bashdb.sourceforge.net/
- "Pro Bash Programming" kirjoittanut Chris Johnson ja Jayant Varma edistyneeseen skriptaukseen.
- ShellCheck staattiseen analyysiin: https://www.shellcheck.net/
