---
title:    "Bash: Pohjakuvion mukaisten merkkien poistaminen"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Miksi
Bash-ohjelmoinnissa on monia tilanteita, joissa haluat poistaa merkkejä, jotka vastaavat tiettyä kaavaa. Tämä voi olla hyödyllistä, jos haluat suodattaa tiettyjä merkkejä tiedostoista tai muuttaa tekstiä haluamallasi tavalla. Tässä blogikirjoituksessa opit miten voit toteuttaa tämän Bashissa käyttäen yksinkertaista komentoa.

## Kuinka tehdä
Voit poistaa merkkejä vastaavan kaavan käyttämällä `tr` komentoa Bashissa. Tehdään käytännön esimerkki, jossa poistamme kaikki numerot tekstistä.

```Bash
# Luo muuttuja, joka sisältää tekstiä, josta haluat poistaa numerot
teksti="Tässä on 123 merkkejä 456 numeroina."

# Käytä tr komentoa poistamaan numerot ja tulosta muutettu teksti
echo $teksti | tr -d '0-9'
```

Tämä tulostaa: `Tässä on merkkejä numeroina.` Käytämme `tr` komentoa parametrilla `-d`, joka määrittelee kaavan, jonka haluamme poistaa. Tässä tapauksessa käytämme `0-9`, joka vastaa kaikkia numeroita 0-9.

Voit myös käyttää `tr` komentoa poistamaan muita merkkejä vastaavan kaavan perusteella. Esimerkiksi, jos haluat poistaa kaikki välimerkit ja erikoismerkit, voit käyttää `-d` parametrilla `[:punct:]`.

## Syvällisempi sukellus
`tr` komento on hyödyllinen monissa Bash-ohjelmoinnin tilanteissa, ei pelkästään merkkien poistamisessa. Voit myös käyttää sitä vaihtamaan merkkejä tai luomaan uusia kaavoja. Voit kokeilla erilaisia kaavoja ja tutkia tarkemmin `tr` komennon toimintaa.

Katso myös
- [tr komentoen opas by Linuxize](https://linuxize.com/post/tr-command-in-linux/)
- [Tr komentoen dokumentaatio by GNU](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- [Komentoriviopas - Bash by ohjelmointi.net](https://ohjelmointi.net/opas/komentorivi/bash/)