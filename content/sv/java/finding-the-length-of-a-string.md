---
title:                "Java: Att hitta längden på en sträng"
simple_title:         "Att hitta längden på en sträng"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Varför

Att kunna hitta längden på en sträng är en grundläggande färdighet inom Java-programmering. Det är en nödvändig kunskap för att kunna manipulera och bearbeta textdata på ett effektivt sätt.

# Hur man gör

För att hitta längden på en sträng i Java kan man använda metoden `length()`. Detta gör det möjligt att räkna antalet tecken i en sträng, inklusive mellanslag och specialtecken.

```Java
String text = "Hej! Detta är en teststräng.";

int längd = text.length();

System.out.println("Strängen " + text + " består av " + längd + " tecken.");
```

Detta kommer att ge följande output:

```
Strängen Hej! Detta är en teststräng. består av 27 tecken.
```

# Djupdykning

Vad många kanske inte vet är att längden på en sträng i Java egentligen mäts i antalet kodenheter (Unicode-tecken) istället för i antalet faktiska tecken. Detta beror på att Unicode-tecken kan ha en större mängd datalagring jämfört med vanliga tecken.

En annan viktig aspekt att tänka på är att längden på en sträng kan påverkas av vilken teckenkodning som används. Om strängen innehåller tecken från olika teckenkodningar kan det resultera i en annan längd än förväntat.

# Se även

- [Java String API](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
- [Unicode och teckenkodning](https://docs.oracle.com/javase/7/docs/technotes/guides/intl/encoding.doc.html)