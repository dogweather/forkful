---
title:    "Bash: Painettujen komentoriviparametrien lukeminen"
keywords: ["Bash"]
---

{{< edit_this_page >}}

# Miksi?

Bash-ohjelmointi on olennainen taito nykypäivän tietotekniikassa ja sen tärkein osa on komentoriviargumenttien lukeminen. Tämä taito on välttämätön ohjelmien käyttämiseksi ja ohjaamiseksi komentoriviltä.

# Miten

Bash-ohjelmoinnissa on tärkeää osata lukea ja käsitellä komentoriviargumentteja, jotka annetaan ohjelman suorittamisen yhteydessä. Tämä voidaan tehdä helposti käyttämällä "```$1```", "```$2```" jne. muuttujia, jotka vastaavat ohjelman suorittamisen yhteydessä annettuja argumentteja.

```
Bash-ohjelma.sh $1 $2

echo "Ensimmäinen argumentti: $1" 
echo "Toinen argumentti: $2"
```

Kun suoritat tämän Bash-ohjelman antamalla kaksi argumenttia, saat seuraavan tulosteen:
```
Bash-ohjelma.sh argumentti1 argumentti2

Ensimmäinen argumentti: argumentti1 
Toinen argumentti: argumentti2
```

# Syvällinen sukellus

Komentoriviargumenttien lukeminen on tärkeä taito, joka auttaa varsinkin monimutkaisempien ohjelmien suorittamisessa. Bash-ohjelman suoritus voi myös sisältää sisäkkäisiä if-lausekkeita, jolloin eri argumentit voivat vaikuttaa suoritukseen eri tavoin.

Bash-ohjelmoijana voit myös käyttää "```$#```" muuttujaa, joka kertoo kuinka monta argumenttia on annettu suorituksen yhteydessä. Tämä voi olla hyödyllistä esimerkiksi silloin, kun haluat tarkistaa, että tarvittavat argumentit on annettu ennen kuin suoritat jonkin toiminnon.

# Katso myös
- [Bash-argumentit](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameters.html)
- [Linux-tietokoneiden komentorivillä liikkuminen](https://www.ict.kruunusillat.fi/kotilin/vinkkeja/Linux/komentorivi)
- [Bash-ohjelmoinnin perusteet](https://coderefinery.github.io/bash/01-introduction/index.html)