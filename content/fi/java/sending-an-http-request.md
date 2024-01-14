---
title:                "Java: Lähettämällä http-pyyntö"
simple_title:         "Lähettämällä http-pyyntö"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Miksi

HTTP-pyyntöjen lähettäminen on tärkeä osa web-ohjelmointia, sillä se mahdollistaa tiedon lähettämisen ja vastaanottamisen eri verkkosivustojen välillä. Se on erityisen hyödyllistä silloin, kun halutaan hakea tietoja jostakin toisesta verkkopalvelusta tai lähettää dataa sille. 

# Miten

HTTP-pyyntöjen lähettämiseksi Java-koodissa tarvitaan neljä askelta: muodostetaan osoite, luodaan yhteys, lähetetään pyyntö ja vastaanotetaan vastaus. Tässä esimerkissä käytetään Apache HttpClient-kirjastoa.

````Java
// Importataan tarvittavat kirjastot
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.HttpClientBuilder;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.net.URISyntaxException;

// Määritellään osoite, jonne pyyntö lähetetään
String url = "https://www.example.com";

// Luodaan HttpClient-olio
HttpClient client = HttpClientBuilder.create().build();

// Luodaan pyyntöolio ja liitetään siihen osoite
HttpGet request = new HttpGet(url);

// Lähetetään pyyntö ja tallennetaan vastaus HttpResponse-olioon
HttpResponse response = client.execute(request);

// Vastaanotetaan vastauksen sisältö ja luetaan se BufferedReaderillä
BufferedReader reader = new BufferedReader(new InputStreamReader(response.getEntity().getContent()));
StringBuilder responseString = new StringBuilder();
String line;

while ((line = reader.readLine()) != null) {
    responseString.append(line);
}

// Tulostetaan vastaus
System.out.println(responseString.toString());
````

Tämän esimerkkikoodin avulla voidaan lähettää GET-pyyntö haluttuun osoitteeseen ja vastaanottaa sen sisältö. Huomaa, että koodia tulee muokata tarvittaessa eri HTTP-metodien, kuten POST tai PUT, lähettämiseen.

# Syvällisempi perehtyminen

HTTP-pyyntöjen lähettäminen sisältää paljon muitakin ominaisuuksia, kuten pyynnön kustomointi, virheiden käsittely ja käytön tehostaminen. Apache HttpClient-kirjastossa on tarjolla monipuolisia toimintoja ja ominaisuuksia, jotka auttavat näissä tehtävissä. Kannattaa tutustua tarkemmin kirjaston dokumentaatioon ja kokeilla erilaisia toimintoja.

# Katso myös

- [Apache HttpClient -kirjaston dokumentaatio](https://github.com/apache/httpcomponents-client/blob/master/httpclient5/README.md)
- [HTTP-pyyntöjen lähettäminen Java-koodissa - Java Code Geeks](https://www.javacodegeeks.com/2012/09/http-post-in-java.html)
- [HTTP-pyyntöjen lähettäminen Javan peruskirjastoilla - Oracle Docs](https://docs.oracle.com/javase/tutorial/networking/urls/readingWriting.html)