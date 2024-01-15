---
title:                "Verkkosivun lataaminen"
html_title:           "Kotlin: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi
 Miksi haluaisit ladata verkkosivun? No, se voi olla kätevää esimerkiksi jos haluat tallentaa jonkin mielenkiintoisen artikkelin myöhempää lukemista varten, tai jos haluat tehdä datan keräämistä ja analysointia.

## Miten
Voit käyttää Kotlinia hyvin monipuolisesti myös verkkosivujen lataamiseen. Tässä on esimerkki miten voit ladata sivun ja tulostaa sen HTML sisällön:

```Kotlin
val url = "https://www.example.com"
val request = java.net.URL(url).openConnection() as HttpURLConnection

request.requestMethod = "GET"
val responseCode = request.responseCode
// Tarkistetaan että sivu on ladattu onnistuneesti
if (responseCode == HttpURLConnection.HTTP_OK) {
// Käytetään BufferedReader lukemaan läpi sivun sisältö
val reader = BufferedReader(InputStreamReader(request.inputStream))
val response = StringBuilder()
var line: String?
line = reader.readLine()
while (line != null) {
response.append(line)
line = reader.readLine()
}
// Tulostetaan HTML sisältö
println(response.toString())
} else {
println("Virhe ladattaessa sivua. Vastauskoodi: $responseCode")
}
```

Tämä koodi tekee HTTP GET pyynnön haluttuun osoitteeseen ja tulostaa vastauksena saadun HTML sisällön. Voit myös tehdä muita pyyntöjä, kuten POST tai HEAD, ja käsitellä vastauksia sen mukaan. Käytettäessä erilaisia lisäkirjastoja, kuten Jsoup, voit myös käsitellä HTML sivun sisältöä tarkemmin ja esimerkiksi hakea sieltä tiettyjä tietoja.

## Syvällisempi tarkastelu
Sivun lataaminen on vain yksi esimerkki siitä, miten voit hyödyntää Kotlinia verkkoyhteyksien ja datan käsittelyssä. Käyttämällä eri kirjastoja ja malleja, voit toteuttaa esimerkiksi automaattisia tiedonkeruusovelluksia tai luoda omia rajapintoja eri palveluiden kanssa kommunikointiin. Kotlinin avulla voit myös käsitellä monimutkaisempia tehtäviä, kuten asynkronisten pyyntöjen hallitsemista ja virheiden käsittelyä.

## Katso myös
- [Kotlinin viralliset kotisivut](https://kotlinlang.org/)
- [Jsoup kirjaston dokumentaatio](https://jsoup.org/)
- [Kotlinin opetusohjelmat verkkosivujen lataamiseen](https://www.programiz.com/kotlin-programming/download-web-page)