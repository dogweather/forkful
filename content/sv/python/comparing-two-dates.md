---
title:    "Python: Jämförande av två datum"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför?

Att jämföra två datum i Python är en vanlig uppgift inom programmering. Det kan vara användbart för att kontrollera om ett datum är tidigare eller senare än ett annat, eller för att beräkna antalet dagar mellan två datum. Det kan också vara en del av en större applikation eller algoritm. Oavsett syfte är det viktigt att veta hur man jämför två datum för att kunna skriva effektiv och korrekt kod.

## Så här gör du:

Att jämföra två datum i Python är enkelt och kan göras på olika sätt. Ett sätt är att använda inbyggda metoderna `datetime` och `timedelta`.

```Python
from datetime import date, timedelta

# Skapa två datumobjekt
d1 = date(2021, 1, 1)
d2 = date(2021, 12, 31)

# Jämför med <, > eller ==
if d1 < d2:
    print("d1 är tidigare än d2.")
elif d1 > d2:
    print("d1 är senare än d2.")
else:
    print("d1 och d2 är samma datum.")

# Beräkna antal dagar mellan d1 och d2
delta = d2 - d1
print("Det är", delta.days, "dagar mellan d1 och d2.")
```

Output:

```
d1 är tidigare än d2.
Det är 364 dagar mellan d1 och d2.
```

En annan metod är att använda `strftime()` för att omvandla datumet till en sträng och sedan jämföra strängarna.

```Python
from datetime import date

d1 = date(2021, 1, 1)

# Omvandla datumet till en sträng med
# formateringen år-månad-dag
d1_str = d1.strftime('%Y-%m-%d')

# Omvandla strängen tillbaka till ett datumobjekt
# Notera att den nya strängen inte har samma formatering
d2 = date.fromisoformat(d1_str)

print(d1 == d2)  # True
```

Output:

```
True
```

## Deep Dive:

När man jämför två datum i Python är det viktigt att vara medveten om några saker. För det första motsvarar varje datum i Python ett specifikt datum och klockslag. Det betyder att även om två datum verkar lika, kan de fortfarande ha olika klockslag. För att undvika oönskade resultat bör man alltid se till att jämföra ett datum mot det andra på samma sätt, antingen genom att omvandla dem till strängar eller genom att använda samma inbyggda metoder.

För det andra är det viktigt att förstå att Python använder sig av den gregorianska kalendern, vilket innebär att vissa datum kan introducera missvisande resultat. Till exempel kan den 29 februari 2020 anses vara ett senare datum än den 1 mars 2020 eftersom det är senare i kalendern, trots att det egentligen är ett datum tidigare än den 1 mars. För att undvika sådana problem kan man använda sig av `timedelta` för att beräkna antalet dagar mellan två datum istället för att bara jämföra dem.

## Se även:

- [Officiell dokumentation för inbyggda moduler `datetime` och `timedelta`](https://docs.python.org/sv/3/library/datetime.html)
- [En sammanfattning om att jämföra datum i Python](https://www.programiz.com/python-programming/datetime/compare-dates)
- [En diskussion om konsekvenserna av att använda gregorianska kalendern i Python](https://stackoverflow.com/questions/2066555/what-does-baseline-date-mean/2066588#2066588) (engelska)