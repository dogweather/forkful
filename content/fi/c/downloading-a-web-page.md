---
title:                "Web-sivun lataaminen"
aliases:
- fi/c/downloading-a-web-page.md
date:                  2024-02-03T17:56:00.601234-07:00
model:                 gpt-4-0125-preview
simple_title:         "Web-sivun lataaminen"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/downloading-a-web-page.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Web-sivun lataaminen C-kielellä sisältää ohjelmallisesti internetsivun sisällön käsittelemisen ja paikallisesti tallentamisen käsittelyä tai offline-käyttöä varten. Ohjelmoijat usein harjoittavat tätä kuluttaakseen web-palveluita, kaapiakseen web-sisältöä tai ollakseen suoraan vuorovaikutuksessa online-resurssien kanssa sovelluksistaan.

## Miten:

Web-sivun lataamiseksi C-kielellä yksi suosittu lähestymistapa on käyttää libcurl-kirjastoa, joka on tehokas ja siirrettävä asiakaspuolen URL-siirtojen kirjasto. Varmista, että sinulla on libcurl asennettuna ja linkitettynä projektissasi. Tässä on esimerkki, joka demonstroi kuinka käyttää libcurlia web-sivun sisällön lataamiseen:

```c
#include <stdio.h>
#include <curl/curl.h>

size_t write_data(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    size_t written = fwrite(ptr, size, nmemb, stream);
    return written;
}

int main(void) {
    CURL *curl;
    FILE *fp;
    CURLcode res;
    char *url = "http://example.com";
    char outfilename[FILENAME_MAX] = "./downloaded_page.html";

    curl = curl_easy_init(); // Alusta libcurl helppo istunto
    if (curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data); // Takaisinkutsu vastaanotetun datan kirjoittamiseen
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp); // Aseta tiedostopiste kirjoittamaan dataa

        res = curl_easy_perform(curl); // Suorita tiedoston lataus
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));
        }

        /* aina siivoa jälkesi */
        curl_easy_cleanup(curl); // Siivoa helppo istunto
        fclose(fp); // Sulje tiedostovirta
    }
    return 0;
}
```
Esimerkkituloste (ei näkyvää tulostetta konsolissa): Tämä koodi lataa sisällön määritetyssä URL:ssa ja tallentaa sen tiedostoon nimeltä `downloaded_page.html`. Tarkista ohjelmasi hakemisto tämän tiedoston löytämiseksi ja nähdäksesi ladatun sisällön.

## Syväsukellus:

Historiallisesti web-sisällön lataaminen C:llä oli hankalampaa, vaatien manuaalista socket-ohjelmointia ja HTTP-protokollan käsittelyä. Libcurl abstrahoi nämä monimutkaisuudet tarjoten kestävän ja korkean tason API:n datan siirtoon webin yli.

Vaikka libcurl yksinkertaistaa HTTP-pyyntöjä C:ssä, modernit ohjelmointikielet kuten Python `requests`-kirjaston kanssa tai JavaScript (Node.js) erilaisten HTTP-asiakaskirjastojen kanssa saattavat tarjota intuitiivisempaa syntaksia ja sisäänrakennettua tukea JSON:ille ja muille web-viestinnässä yleisesti käytetyille datamuodoille. Kuitenkin C ja libcurl tarjoavat korkean suorituskyvyn ja vakaan ratkaisun järjestelmiin, joissa tehokkuus, hienojakoiset hallintamahdollisuudet tai integraatio olemassa oleviin C-koodikantoihin ovat kriittisiä. On myös huomionarvoista, että C:tä yhdistettynä libcurlin kanssa voidaan käyttää paljon muuhunkin kuin vain web-sivujen lataamiseen - se pystyy FTP:hen, SMTP:hen ja paljon muuhun, tehden siitä monipuolisen työkalun ohjelmoijan työkalupakissa.
