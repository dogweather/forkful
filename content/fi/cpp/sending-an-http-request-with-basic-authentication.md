---
title:                "Lähettämällä http-pyyntö perusautentikoinnilla"
html_title:           "C++: Lähettämällä http-pyyntö perusautentikoinnilla"
simple_title:         "Lähettämällä http-pyyntö perusautentikoinnilla"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?
HTTP-pyyntöjen lähettäminen perustetunnistuksen kanssa on tärkeä osa verkkokehitystä. Se mahdollistaa suojatun tietojen vaihdon palvelimien välillä ja auttaa varmistamaan, että vain oikeutetut käyttäjät pääsevät tiettyihin resursseihin.

# Kuinka?
Käyttämällä C++ -ohjelmointikieltä voit lähettää HTTP-pyynnön perustetunnistuksella helposti ja turvallisesti. Alla on yksinkertainen koodinäyte, joka näyttää, miten se tehdään:

```C++
#include <iostream>
#include <curl/curl.h> //Tarvittavat kirjastot

int main(){
  CURL *curl;
  CURLcode res;
  
  //Alusta cUrl
  curl = curl_easy_init();
  
  if(curl) {
    //Aseta pyyntöosoite
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");
    
    //Aseta tunnistustiedot
    curl_easy_setopt(curl, CURLOPT_USERPWD, "käyttäjänimi:salasana");
    
    //Lähetä HTTP-pyyntö
    res = curl_easy_perform(curl);
    
    //Tarkista vastauskoodi
    if(res == CURLE_OK){
      //Tulosta vastaus
      std::cout << "Vastaus saatu onnistuneesti!" << std::endl;
    } else {
      //Tulosta virheilmoitus
      std::cout << "Virhe: " << curl_easy_strerror(res) << std::endl;
    }
    
    //Vapauta curl
    curl_easy_cleanup(curl);
  } else {
    //Jos cUrl on alustettu virheellisesti, tulosta virheilmoitus
    std::cerr << "cUrl ei alustettu!" << std::endl;
  }
  
  return 0;
}
```

Kun suoritat koodin, tuloste näyttää vastauksen, jos pyyntö onnistuu, tai virheilmoituksen, jos jokin menee pieleen.

# Syväsukellus
Perustetunnistuksen käyttö HTTP-pyynnöissä on ollut käytössä jo pitkään ja se on yksi yleisimmistä tavoista suojata tietojen vaihto verkkopalvelimien välillä. On myös muita vaihtoehtoja, kuten OAuth, joka tarjoaa lisäturvaa ja mahdollisuuden antaa rajoitettu pääsy tiettyihin resursseihin.

C++:ssa perustetunnistuksen käyttöön tarvitaan cUrl-kirjasto, joka tarjoaa käyttöliittymän verkkoprotokollien käyttämiseen. Tällöin tarvittavaa funktiota kutsutaan asettamalla URL, käyttäjänimi ja salasana parametreiksi ja suorittamalla pyyntö.

# Katso myös
https://curl.haxx.se/docs/httpscripting.html - Lisää tietoa HTTP-pyyntöjen lähettämisestä cUrl:lla.

https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication - Tietoa erilaisista verkkotunnistautumismenetelmistä.