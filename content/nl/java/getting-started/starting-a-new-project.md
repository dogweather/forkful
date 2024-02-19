---
aliases:
- /nl/java/starting-a-new-project/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:18.828145-07:00
description: "Een nieuw Java-project starten is alsof je een nieuw doek klaarzet voor\
  \ je meesterwerk. Programmeurs zetten nieuwe projecten op om idee\xEBn om te zetten\
  \ in\u2026"
lastmod: 2024-02-18 23:09:01.713817
model: gpt-4-0125-preview
summary: "Een nieuw Java-project starten is alsof je een nieuw doek klaarzet voor\
  \ je meesterwerk. Programmeurs zetten nieuwe projecten op om idee\xEBn om te zetten\
  \ in\u2026"
title: Een nieuw project starten
---

{{< edit_this_page >}}

## Wat & Waarom?

Een nieuw Java-project starten is alsof je een nieuw doek klaarzet voor je meesterwerk. Programmeurs zetten nieuwe projecten op om ideeën om te zetten in functionele software, en elke nieuwe start is een stap richting innovatie of het oplossen van een probleem.

## Hoe te:

Laten we beginnen. We gaan een eenvoudig Java-project creëren met de opdrachtregel en een klassiek "Hallo, Wereld!" programma compileren en uitvoeren.

Maak eerst een directory voor je project en navigeer ernaar:

```bash
mkdir MijnJavaProject
cd MijnJavaProject
```

Maak nu je Java-bestand:

```bash
echo 'public class HelloWorld { public static void main(String[] args) { System.out.println("Hallo, Wereld!"); }}' > HelloWorld.java
```

Tijd om te compileren:

```bash
javac HelloWorld.java
```

Voer je meesterwerk uit:

```bash
java HelloWorld
```

Voilà! De console zou moeten uitvoeren:

```java
Hallo, Wereld!
```

## Diepgaand:

Ooit werden Java-projecten handmatig beheerd, een beetje als jongleren met bestanden in een circus. Tegenwoordig hebben we tools zoals Maven en Gradle om het alledaagse werk te automatiseren.

Maven heeft bijvoorbeeld de standaard projectindeling gedefinieerd waarmee de meeste Java-ontwikkelaars vandaag de dag bekend zijn. Het regelt ook afhankelijkheden zodat je niet handmatig jars hoeft te downloaden en je geen nachtmerries over de classpath hoeft te hebben.

Gradle kwam later op het toneel, bood meer flexibiliteit en gebruikte een op Groovy-gebaseerde DSL (Domain Specific Language) voor scripting. Het is zoals Maven, maar dan met meer vrijheid voor aangepaste scripts zonder extra plugins.

Alternatieven? Zeker, er is Ant met Ivy, maar dat is een beetje ouderwets, zoals luisteren naar muziek op een cassettebandje. Je moet van de nostalgie houden, maar het is misschien niet voor iedereen in dit tijdperk van streamingdiensten.

Wanneer je een nieuw Java-project start, denk dan na over hoe groot en complex het zal worden. Voor leren of kleine projecten werkt handmatig beheer prima. Maar als je van plan bent iets substantieels te bouwen of in een team te werken, dan is een buildtool de weg te gaan.

## Zie Ook:

Om een voorsprong te krijgen in het gebruik van buildtools, bekijk de volgende bronnen:

- [Maven Getting Started Guide](https://maven.apache.org/guides/getting-started/index.html)
- [Building Java Projects with Gradle](https://spring.io/guides/gs/gradle/)
- [Inleiding tot Ant](https://ant.apache.org/manual/index.html)

En voor diegenen die dieper willen duiken in de nieuwe functies van JDK, de [Java Platform, Standard Edition Oracle Documentatie](https://docs.oracle.com/en/java/javase/index.html) is een schatkist.
