---
date: 2024-01-26 00:37:19.924186-07:00
description: "Hur man g\xF6r: L\xE5t oss s\xE4ga att din Arduino l\xE4ser av en sensor\
  \ som ibland kan producera v\xE4rden utanf\xF6r omr\xE5det. S\xE5 h\xE4r kan du\
  \ hantera det."
lastmod: '2024-03-13T22:44:38.174691-06:00'
model: gpt-4-1106-preview
summary: "L\xE5t oss s\xE4ga att din Arduino l\xE4ser av en sensor som ibland kan\
  \ producera v\xE4rden utanf\xF6r omr\xE5det."
title: Hantering av fel
weight: 16
---

## Hur man gör:
Låt oss säga att din Arduino läser av en sensor som ibland kan producera värden utanför området. Så här kan du hantera det:

```Arduino
int sensorValue = analogRead(A0);

if (sensorValue >= 0 && sensorValue <= 1023) {
  // Värdet är inom området, fortsätt med bearbetning
  Serial.println(sensorValue);
} else {
  // Värdet är utanför området, hantera felet
  Serial.println("Fel: Sensorns värde är utanför området.");
}
```
Exempelutmatning:
```
523
Fel: Sensorns värde är utanför området.
761
```

## Fördjupning
Felhantering har inte alltid varit så rakt på sak. I de tidiga dagarna ignorerade utvecklare ofta fel, vilket ledde till den fruktade "odefinierade beteendet". När programmeringen utvecklades, gjorde även verktygen det – nu har du undantag (exceptions) i många språk, men i Arduino-världen är det ofta fortfarande en gammaldags 'kolla-först' på grund av hårdvarubegränsningar och C++-rötter.

I Arduino-programmering ser man ofta `if-else`-satser för felhantering. Men det finns alternativ: använda `assert`-funktionen för att stoppa körningen om ett villkor misslyckas eller att utforma inbyggda säkerhetsåtgärder i själva hårdvaruuppsättningen.

När du implementerar felhantering, överväg effekten av att stoppa programmet versus att tillåta det att fortsätta i ett standard- eller säkert tillstånd. Det finns en avvägning, och det rätta valet beror på potentiell skada av avbrott kontra felaktig drift.

## Se också
Förbättra din kunskap om feldetektering och hantering med dessa:

- Arduino språkreferens: https://www.arduino.cc/reference/en/
- Embedded Artistrys fördjupade titt på felhantering: https://embeddedartistry.com/blog/2017/05/17/creating-a-circular-buffer-in-c-and-c/
- C++ Felhantering: https://en.cppreference.com/w/cpp/error/exception

Detta ska ge dig kunskapen och förtroendet att undvika fallgroparna med fel i dina Arduino-äventyr.
