---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:07.965500-07:00
description: "Een debugger gebruiken betekent het inzetten van een hulpmiddel om te\
  \ testen en fouten in je code te herstellen. Programmeurs doen dit om de stroom\
  \ van\u2026"
lastmod: '2024-03-11T00:14:24.507350-06:00'
model: gpt-4-0125-preview
summary: "Een debugger gebruiken betekent het inzetten van een hulpmiddel om te testen\
  \ en fouten in je code te herstellen. Programmeurs doen dit om de stroom van\u2026"
title: Een debugger gebruiken
---

{{< edit_this_page >}}

## Wat & Waarom?
Een debugger gebruiken betekent het inzetten van een hulpmiddel om te testen en fouten in je code te herstellen. Programmeurs doen dit om de stroom van hun applicaties te begrijpen, de bronnen van fouten aan te wijzen, en de logica tijdens de uitvoering te verifiëren.

## Hoe:
Stel je voor dat je een eenvoudig Java-programma hebt dat problemen veroorzaakt, en je kunt niet uitvogelen waarom. Zo zou je een debugger starten met Eclipse, een van de populaire IDE's voor Java-ontwikkeling:

Zorg eerst dat je een breekpunt hebt ingesteld. Klik dan met de rechtermuisknop op het bestand, selecteer 'Debug As', en klik op 'Java Application'.

```Java
public class DebugExample {
    public static void main(String[] args) {
        int a = 5;
        int b = 0;
        // Stel hier een breekpunt in
        int resultaat = divide(a, b);
        System.out.println("Het resultaat is: " + resultaat);
    }

    private static int divide(int teller, int noemer) {
        // Nog een goede plek voor een breekpunt
        return teller / noemer;
    }
}
```

Door dit te doen, zal je programma pauzeren bij het breekpunt, en je kunt variabelen inspecteren, regel voor regel door de code stappen, en observeren hoe je programma zich gedraagt.

Voorbeelduitvoer (in een debuggerconsole):
```
Breekpunt bereikt op regel: int resultaat = divide(a, b);
```

## Diepgaand
Het concept van debuggen bestaat al sinds de vroege dagen van programmeren. Er gaat een legende dat de term "bug" eigenlijk afkomstig is van een echte mottige bug die gevonden werd in een computer door Grace Hopper, een pionier op het gebied. Fast forward naar vandaag de dag, en we hebben geavanceerde IDE's zoals IntelliJ IDEA, Eclipse, en NetBeans die krachtige debuggers bevatten.

Alternatieven voor IDE debuggers omvatten loggen, printstatements (arme-mans-debugger), beweringen en zelfstandige debughulpmiddelen zoals jdb (Java Debugger) die deel uitmaakt van de Java Development Kit (JDK).

Een debugger werkt door de programmeur in staat te stellen de uitvoering te pauzeren (breekpunten), door de code te stappen, variabele waarden te inspecteren, die waarden ter plekke te wijzigen, en zelfs blok voor blok code uit te voeren. Het gebruik van een debugger wordt vaak beschouwd als een onschatbare techniek voor het ontwikkelen van complexe applicaties waarbij het opsporen van de exacte regel code die een probleem veroorzaakt, kan worden vergeleken met het vinden van een naald in een hooiberg.

## Zie Ook
- De officiële Oracle-documentatie over debuggen: [Oracle Java SE Debugging](https://docs.oracle.com/javase/8/docs/technotes/tools/windows/jdb.html)
- De gids van Eclipse over debuggen: [Eclipse Debugging Tips](https://www.eclipse.org/community/eclipse_newsletter/2017/june/article4.php)
- VisualVM, een visueel hulpmiddel dat verschillende command-line JDK-gereedschappen en lichtgewicht profileringsmogelijkheden integreert: [VisualVM](https://visualvm.github.io/)
