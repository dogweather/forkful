---
date: 2024-01-26 03:37:03.135660-07:00
description: "Refaktorisering \xE4r processen att omstrukturera befintlig dator kod\u2014\
  \xE4ndra faktoriseringen\u2014utan att \xE4ndra dess yttre beteende. Programmerare\
  \ g\xF6r det f\xF6r att\u2026"
lastmod: '2024-03-13T22:44:37.491369-06:00'
model: gpt-4-0125-preview
summary: "Refaktorisering \xE4r processen att omstrukturera befintlig dator kod\u2014\
  \xE4ndra faktoriseringen\u2014utan att \xE4ndra dess yttre beteende."
title: Refaktorisering
weight: 19
---

## Vad & Varför?
Refaktorisering är processen att omstrukturera befintlig dator kod—ändra faktoriseringen—utan att ändra dess yttre beteende. Programmerare gör det för att städa upp koden, förbättra läsbarheten och göra den lättare att underhålla och utöka, allt utan att lägga till nya funktioner.

## Hur man gör:
Anta att du har en bit kod som beräknar och skriver ut arean och omkretsen för en rektangel utifrån dess längd och bredd. Det gör jobbet, men det är upprepande och lite rörigt.

```python
# Originalversion
längd = 4
bredd = 3

# Beräkna area och omkrets
area = längd * bredd
omkrets = 2 * (längd + bredd)

print("Area:", area)
print("Omkrets:", omkrets)
```

Vi kan refaktorisera detta genom att inkapsla funktionaliteten i funktioner, vilket gör koden mer organiserad och återanvändbar:

```python
# Refaktorerad Version

def beräkna_area(längd, bredd):
    return längd * bredd

def beräkna_omkrets(längd, bredd):
    return 2 * (längd + bredd)

# användning
längd = 4
bredd = 3

print("Area:", beräkna_area(längd, bredd))
print("Omkrets:", beräkna_omkrets(längd, bredd))
```

Båda kodsnuttarna ger samma resultat:
```
Area: 12
Omkrets: 14
```

Men den refaktorerade versionen är renare och separerar ansvarsområden, vilket gör det lättare att uppdatera en beräkning utan att påverka den andra.

## Djupdykning
Refaktorisering har sina rötter i de tidiga dagarna av programvaruutveckling när programmerare insåg att kod kunde—och borde—förbättras även om den redan "fungerar". Martin Fowlers banbrytande bok "Refaktorisering: Förbättring av befintlig kods design" formulerade många grundläggande principer och tekniker. Han sade berömt, "Vilken dåre som helst kan skriva kod som en dator kan förstå. Bra programmerare skriver kod som människor kan förstå."

Alternativ till refaktorisering kan inkludera att skriva om koden från grunden eller göra mindre justeringar utan systematisk förbättring. Dock är refaktorisering vanligtvis mer kostnadseffektiv än en omskrivning och mindre riskabel än ad-hoc-modifieringar. Implementeringsdetaljer kan vara specifika för varje programmeringsparadigm; dock lämpar sig objektorienterad programmering särskilt väl för refaktorisering, speciellt med tekniker som extrahering av metoder (som våra `beräkna_area` och `beräkna_omkrets` funktioner), inlining, flyttning av funktioner mellan objekt och namnändring av metoder eller variabler för tydlighet.

Refaktorisering i Python använder ofta verktyg som `PyCharm`, som har inbyggda refaktoriseringskapaciteter, eller `rope`, ett Python-bibliotek specifikt utformat för refaktorisering. Försiktig användning av versionskontroll, såsom `git`, under refaktorisering rekommenderas starkt för att hålla koll på ändringar stegvis.

## Se också
För de som är hungriga efter mer, dyk in i följande resurser:
- Martin Fowlers bok: [Refaktorisering: Förbättring av befintlig kods design](http://www.refactoring.com/)
- Python-refaktorisering med `rope`: [GitHub - rope](https://github.com/python-rope/rope)
- PyCharms refaktoriseringsdokumentation: [Jetbrains PyCharm Refactoring Source Code](https://www.jetbrains.com/help/pycharm/refactoring-source-code.html)
- Refactoring.guru: [Refaktorisering och Designmönster](https://refactoring.guru/refactoring)
- Clean Code-föreläsningar av Uncle Bob (Robert C. Martin): [Clean Code - Uncle Bob / Lektion 1](https://www.youtube.com/watch?v=7EmboKQH8lM)
