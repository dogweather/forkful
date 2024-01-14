---
title:    "Bash: Tulevan tai menneen päivämäärän laskeminen"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Monet ihmiset saattavat ihmetellä miksi heidän pitäisi laskea tietty päivämäärä tulevaisuuteen tai menneisyyteen. Yksi yleinen syy tähän on, että he haluavat suunnitella tulevia tapahtumia tai tehdä merkintöjä menneistä tapahtumistaan. Bash ohjelmointikielen avulla tämä on helppoa ja nopeaa tehdä.

## Miten

Laskemalla päivämääriä tulevaisuuteen tai menneisyyteen Bashilla on monia erilaisia tapoja. Yksi yleisimmistä tavoista on käyttää "date" komentoa. Voit käyttää sitä esimerkiksi näin:

```Bash
date -d "next Monday"
```

Tämä tulostaa seuraavan maanantain päivämäärän nykyhetkestä. Voit myös määrittää minkä tahansa päivämäärän haluat lisäämällä tai vähentämällä päiviä.

```Bash
date -d "4 days"
```

Tämä tulostaa päivämäärän neljän päivän päästä nykyhetkestä. Voit myös määrittää haluamasi päivämäärän tietyn muodon mukaan, esimerkiksi päiväyksen muodossa.

```Bash
date -d "2021-12-25"
```

Tämä tulostaa joulupäivän päivämäärän vuonna 2021. Voit lisätä näihin komentoihin myös muita optioita, kuten ajan tai aikavyöhykkeen. Kokeile rohkeasti erilaisia vaihtoehtoja ja löydä itsellesi sopiva tapa laskea päivämääriä tulevaisuuteen tai menneisyyteen.

## Syvällinen sukellus

Bashilla on muitakin tapoja laskea päivämääriä tulevaisuuteen tai menneisyyteen kuin "date" komennon käyttö. Voit esimerkiksi käyttää "expr" komentoa laskemaan päiviä ja päivämääriä. Voit myös käyttää "let" tai "bc" komentoa laskemaan monimutkaisempia päivämääriä. Tarkempien ohjeiden ja esimerkkien löytämiseksi kannattaa tutustua Bashin manuaalisivuihin.

## Katso myös

- [Bashin manuaalisivut](https://www.gnu.org/software/bash/manual/bash.html)
- [LinuxCommand.org: Date and Time Manipulation in Linux Bash](https://linuxcommand.org/lc3_lts0080.php)
- [The Linux Documentation Project: Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/datecalc.html)