---
title:                "Lukemalla komentorivin argumentit"
html_title:           "Gleam: Lukemalla komentorivin argumentit"
simple_title:         "Lukemalla komentorivin argumentit"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Mikä & Miksi?
Komentoriviparametrien lukeminen tarkoittaa sitä, että ohjelma pystyy ottamaan käyttäjän tarjoamat parametrit ja käyttämään niitä ohjelman suorittamiseen. Ohjelmoijat tekevät tätä jotta heidän ohjelmansa olisi joustavampi ja käyttäjäystävällisempi, sillä käyttäjät voivat säätää ohjelman toimintaa käyttämällä erilaisia parametreja.

Miten?
Gleamilla on mahdollista lukea komentoriviparametreja käyttämällä moduulia "os". Käyttämällä funktiota "arguments" ohjelmoijat voivat tallentaa parametrit muuttujaan ja käyttää niitä tarvittaessa. Alla on esimerkkejä koodista ja siihen liittyvistä tulosteista:

```Gleam
import os

fn main() {
    let arguments = os.arguments()
    // Käyttäjä antoi parametrina "true"
    // arguments arvo olisi siis: ["true"]
    // Voit käyttää parametria esim. tarkistamalla sen arvoa if-lauseella.

    if arguments == ["true"] {
        // Tee jotain
    }
}

```

## Syvemmälle
Komennoriviparametrien lukeminen on ollut osa ohjelmointia jo pitkään ja se on yleinen käytäntö monissa ohjelmointikielissä. Jotkut vaihtoehtoiset menetelmät ovat esimerkiksi ympäristömuuttujien käyttäminen tai konfiguraatiotiedostojen lukeminen. Gleamissa komentoriviparametrien lukeminen on helppoa ja kätevää käyttämällä moduulia "os".

## Katso myös
Voit tutustua tarkemmin parametrien lukemiseen Gleamissa ja moduuliin "os" Gleamin virallisessa dokumentaatiossa. Näin pysyt ajan tasalla uusista ominaisuuksista ja parhaista käytännöistä. https://gleam.run/documentation/

Lopputulos
Nyt tiedät, mitä komentoriviparametrien lukeminen tarkoittaa ja miksi ohjelmoijat tekevät sitä. Tutustuit myös Gleamin tarjoamaan helpoon ja kätevään tapaan lukea parametreja. Jatkossa voit hyödyntää tätä ominaisuutta omassa ohjelmoinnissasi ja tehdä ohjelmistasi vieläkin monipuolisempia ja käyttäjäystävällisempiä.