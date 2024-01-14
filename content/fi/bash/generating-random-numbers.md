---
title:    "Bash: Satunnaislukujen luominen"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Miksi

Sattumanvaraisten lukujen generointi voi olla hyödyllistä esimerkiksi silloin kun haluat testata koodiasi satunnaisilla syötteillä tai luoda arpajaisnumeroita. Se voi myös olla hauska tapa luoda erilaisia pelejä tai sovelluksia.

## Miten

Bashilla voit helposti generoida satunnaisia lukuja käyttämällä `$RANDOM` muuttujaa. Se palauttaa satunnaisen luvun väliltä 0-32767 joka kerta kun sitä kutsutaan.

```Bash
echo $RANDOM # Tulostaa esimerkiksi 19526
```

Voit myös rajoittaa luvun välille jota haluat käyttää. Esimerkiksi jos haluat satunnaisen luvun väliltä 1-10, voit käyttää seuraavaa koodia:

```Bash
echo $((RANDOM % 10 + 1)) # Tulostaa satunnaisen luvun väliltä 1-10
```

## Syvempi syvennys

Bashissa satunnaislukuja generoitaessa käytetään pohjalla Unixin `rand()` funktiota. Tämä funktio käyttää hyödykseen tietokoneen kellonajan tarkkaa alkuarvoa, jotta ei generoida kahta samaa satunnaislukua peräkkäin. Tämä takaa sen, että saamme todella satunnaisia lukuja.

On myös hyvä huomata, että `$RANDOM` muuttuja palauttaa vain kokonaislukuja. Jos haluat generoida desimaalilukuja, voit käyttää apuna `awk` ohjelmaa. Esimerkiksi alla oleva koodi palauttaa satunnaisen desimaaliluvun väliltä 0-1:

```Bash
echo | awk '{ srand(); print rand() }' # Tulostaa esimerkiksi 0.54342
```

## Katso myös

- [The Linux Documentation Project: Bash Guide for Beginners](http://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [GNU Bash Manuaali](https://www.gnu.org/software/bash/manual/bash.html)
- [Bash-koodaus esimerkkejä](https://tldp.org/LDP/abs/html/sample-bashrc.html)