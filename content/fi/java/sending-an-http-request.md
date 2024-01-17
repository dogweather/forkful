---
title:                "Http-pyynnön lähettäminen"
html_title:           "Java: Http-pyynnön lähettäminen"
simple_title:         "Http-pyynnön lähettäminen"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

Mikä & Miksi?

HTTP-pyyntöjen lähettäminen on tapa, jolla ohjelmoijat kommunikoivat tietokonejärjestelmien kanssa Internetin välityksellä. Tämä mahdollistaa tietojen hakemisen, lähettämisen ja käsittelyn eri lähteistä. Ohjelmoijat lähettävät HTTP-pyynnön usein esimerkiksi tiedon hakemiseksi verkkosivuilta.

Miten:

```java 
import java.net.*; 

public class HTTPRequestExample { 

public static void main(String[] args) { 

try { 

// Luodaan URL-olio ja yhdistetään se haluttuun osoitteeseen 
URL url = new URL("https://www.example.com/"); 

// Luodaan URL-yhteys ja lähettetään GET-pyyntö 
HttpURLConnection con = (HttpURLConnection) url.openConnection(); 
con.setRequestMethod("GET"); 

// Saadaan vastauskoodi 
int responseCode = con.getResponseCode(); 
System.out.println("Vastauskoodi: " + responseCode); 

// Luetaan vastausasetta ja tulostetaan se 
BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream())); 
String inputLine; 
StringBuffer response = new StringBuffer(); 

while ((inputLine = in.readLine()) != null) { 
response.append(inputLine); 
} 
in.close(); 

// Tulostetaan vastaus 
System.out.println(response.toString()); 

} catch (Exception e) { 
e.printStackTrace(); 
} 

} 
} 

```

Tulostus: 
Vastauskoodi: 200 
<!DOCTYPE html> 
<html> 
<head> 
<title> Esimerkkisivu </title> 
</head> 

Deep Dive:

HTTP-pyyntöjen lähettäminen on ollut tärkeä osa Internetin kehitystä, ja se perustuu protokollaan, joka mahdollistaa tehokkaan tiedon siirron eri järjestelmien välillä. On olemassa myös muita vaihtoehtoja, kuten FTP- ja SMTP-pyyntöjä, mutta HTTP on tullut suosituimmaksi teknologiaksi verkkosivuilla käytetyille tiedonhakuyhteyksille.

See Also:

- [HTTP-pyynnöt Java - Lucinda L shykvtluvdidkei](https://www.google.com/search?q=http+pyynnot+java&oq=http+pyynnot+java&aqs=chrome..69i57j69i64l3.2832j0j7&sourceid=chrome&ie=UTF-8) 
- [Oracle Java Tietosanakirja - Java.net](https://www.google.com/search?q=oracle+java+tietosanakirja+java.net&oq=oracle+java+tietosanakirja+java.net&aqs=chrome..69i57j69i64j69i60l2j69i65j69i60.3717j0j7&sourceid=chrome&ie=UTF-8)