---
title:                "Uuden projektin aloittaminen"
aliases:
- fi/java/starting-a-new-project.md
date:                  2024-01-20T18:04:00.724342-07:00
model:                 gpt-4-1106-preview
simple_title:         "Uuden projektin aloittaminen"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Aloittaa uuden projektin Java-ohjelmoinnissa tarkoittaa tyhjästä aloittamista; uutta koodikantaa, uusia ideoita, uutta energiaa. Koodarit aloittavat uusia projekteja ratkaistakseen ongelmia, oppiakseen, tai vain kokeillakseen jotain uutta ja jännittävää.

## How to: (Kuinka tehdä:)
```java
// Luo 'ExampleProject' hakemiston
mkdir ExampleProject

// Siirry luotuun hakemistoon
cd ExampleProject

// Alusta uusi Git-repo
git init

// Luo uusi Java-projekti käyttäen Maven:
mvn archetype:generate -DgroupId=com.example -DartifactId=example-project -DarchetypeArtifactId=maven-archetype-quickstart -DinteractiveMode=false
```
Kun olet suorittanut yllä olevat komennot, sinulla on uusi Java-projekti 'example-project' kansiossa, joka sisältää standardin Maven-kansiokaavion ja yksinkertaisen `App.java` aloitusluokan.

## Deep Dive (Syväsukellus)
Historiallisesti Java-projektin aloittaminen saattoi olla monimutkaisempaa. Vaihtoehtoja oli vähemmän. Nykyään on useita työkaluja, kuten Maven ja Gradle, jotka automatisoivat projektiin liittyvät rutiinit. Ne tarjoavat projektikarttoja (archetypes), joista voit valita.

Maven on ollut Java-maailman kivijalka pitkään. Se hallinnoi kirjastoriippuvuuksia, elinkaarta ja paljon muuta. Gradle tarjoaa vastaavan toiminnallisuuden, mutta joustavamman build-skriptauksen ja paremman suorituskyvyn.

Implementation detail: Java-projektin aloittaminen Mavenilla tapahtuu `mvn archetype:generate` -komennolla, joka luo uuden projektin perustuen valittuun archetypeen. Mikä on archetype? Se on projektin runko, pohja. Kukin archetype sisältää valmiin tiedostorakenteen ja perustiedostot, kuten `pom.xml` Mavenin asetustiedoston.

## See Also (Katso myös)
- [Maven Getting Started Guide](https://maven.apache.org/guides/getting-started/)
- [Gradle Guides](https://gradle.org/guides/)
