---
title:                "Hantering av fel"
date:                  2024-01-26T00:37:19.924186-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hantering av fel"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/handling-errors.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Felhantering i dina program fångar de oväntade ting som försöker sätta käppar i hjulet. Du gör det för att hålla din Arduino från att smälta ned när det oväntade inträffar.

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
