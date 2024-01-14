---
title:                "Java: Verkkosivun lataaminen."
simple_title:         "Verkkosivun lataaminen."
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi

Internetin sisällön lataaminen voi olla hyödyllinen taito monille, joko tiettyjen tietojen kaivamiseen tai yksinkertaisesti säästämään aikaa ja vaivaa manuaalisen kopiointiin. Java tarjoaa tehokkaan tavan ladata verkkosivuja ja hyödyntää niiden sisältöä.

## Ohje

Ensimmäinen askel on ottaa käyttöön Java-kehitysympäristö, kuten Eclipse tai Netbeans. Seuraavaksi tarvitaan paketti, joka pystyy lähettämään HTTP-pyynnön ja vastaanottamaan vastauksen verkkosivulta. Tässä esimerkissä käytämme Apache HttpComponents -pakettia.

```Java
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
```

Näiden pakettien avulla voimme luoda yhteyden verkkosivuun ja ladata sen sisällön. Esimerkiksi lataamme Googlen etusivun:

```Java
// Luodaan HTTP-yhteydenhallintapalvelu
CloseableHttpClient httpClient = HttpClients.createDefault();

// Luodaan Get-pyyntö ja asetetaan URL
HttpGet request = new HttpGet("https://www.google.com");

// Lähetetään pyyntö ja vastaanotetaan vastaus
CloseableHttpResponse response = httpClient.execute(request);

// Haetaan vastauksen sisältö HTML-muodossa
String content = EntityUtils.toString(response.getEntity());

// Tulostetaan sisältö
System.out.println(content);

// Suljetaan yhteydet
response.close();
httpClient.close();
```

Voimme myös suodattaa vastauksen sisältöä saadaksemme haluamamme tiedon. Esimerkiksi jos haluamme tulostaa vain Googlen hakukentän sisällön:

```Java
// Luodaan hakukentän kuvaussäännös
String searchField = "<input type=\"text\".*?>";

// Luodaan uusi regex-suodatin ja sovelletaan se sisältöön
Pattern p = Pattern.compile(searchField);
Matcher m = p.matcher(content);

// Tulostetaan hakukentän sisältö
if (m.find()) {
  System.out.println(m.group(0));
}
```

Tässä esimerkissä käytimme regex-suodatusta saadaksemme haluamamme tiedon, mutta voit myös käyttää muita kehittyneempiä tapoja kuten HTML-analysointikirjastoja.

## Syvempi sukellus

Internetin sisällön lataaminen voi joskus olla monimutkaisempi prosessi. Esimerkiksi jos sivustolla on sisäänkirjautumisominaisuus tai muuta tietoa, jota haluat simuloida. Tässä tapauksessa voit käyttää kirjastoa, kuten Selenium Webdriver, joka mahdollistaa sivustolla tapahtuvan toiminnan automatisoinnin.

Kaiken kaikkiaan internetin sisällön lataaminen Java-koodilla on nopeaa ja helppoa, ja se tarjoaa paljon mahdollisuuksia hyödyntää verkkosivujen sisältöä.

## Katso myös

- [Apache HttpComponents](https://hc.apache.org/httpcomponents-client-4.5.x/index.html)
- [Selenium Webdriver](https://www.seleniumhq.org/projects/webdriver/)