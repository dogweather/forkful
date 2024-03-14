---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:43.361766-07:00
description: "Associativa arrayer \xE4r som superladdade arrayer som l\xE5ter dig\
  \ anv\xE4nda str\xE4ngar som index ist\xE4llet f\xF6r enbart heltal. Programmerare\
  \ anv\xE4nder dem f\xF6r mer\u2026"
lastmod: '2024-03-13T22:44:38.073543-06:00'
model: gpt-4-0125-preview
summary: "Associativa arrayer \xE4r som superladdade arrayer som l\xE5ter dig anv\xE4\
  nda str\xE4ngar som index ist\xE4llet f\xF6r enbart heltal. Programmerare anv\xE4\
  nder dem f\xF6r mer\u2026"
title: "Att anv\xE4nda associativa arrayer"
---

{{< edit_this_page >}}

## Vad & Varför?

Associativa arrayer är som superladdade arrayer som låter dig använda strängar som index istället för enbart heltal. Programmerare använder dem för mer komplexa datastrukturer, vilket gör det enklare att hantera data som inte passar snyggt in i en sekventiell lista.

## Hur gör man:

Börja med att deklarera en associativ array i Bash:

```Bash
declare -A my_array
```

Sedan kan du börja fylla den med värden, med strängar som nycklar:

```Bash
my_array["namn"]="Linux Journal"
my_array["ämne"]="Programmering"
```

För att komma åt ett element, använd dess nyckel:

```Bash
echo ${my_array["namn"]}  # Ger ut: Linux Journal
```

Att iterera över nycklar och värden är också enkelt:

```Bash
for key in "${!my_array[@]}"; do
    echo "$key: ${my_array[$key]}"
done
```

Exempel på utskrift kan se ut så här:

```
namn: Linux Journal
ämne: Programmering
```

För att lägga till eller modifiera element, tilldela helt enkelt ett värde till en nyckel, liknande den inledande populationen:

```Bash
my_array["läsare"]="Du"
```

Och för att ta bort ett element, använd `unset`:

```Bash
unset my_array["ämne"]
```

## Djupdykning

Associativa arrayer introducerades i Bash version 4.0, vilket gör dem till en relativt ny tillägg till språket. Innan deras introduktion var hanteringen av arrayer med icke-heltalsindex besvärlig, ofta krävande kringgående lösningar eller externa verktyg som `awk` eller `sed`.

Bakom kulisserna implementerar Bash associativa arrayer med hjälp av hashtabeller. Denna implementering möjliggör effektiv nyckelsökning, som förblir ganska konstant oavsett storleken på arrayen, en kritisk egenskap för prestanda i skriptexekvering.

Även om associativa arrayer i Bash tillför mycket kraft och flexibilitet till shellskriptning, kommer de med sin egen uppsättning begränsningar, såsom att vara något mer omständliga att arbeta med jämfört med arrayer i högre programspråk som Python eller JavaScript. För komplexa uppgifter för datahantering kan det fortfarande vara värt att överväga externa verktyg eller språk som är bättre lämpade för jobbet.

Men, för många typiska skriptuppgifter, tillhandahåller associativa arrayer ett värdefullt verktyg i Bash-programmerarens verktygslåda, vilket möjliggör mer läsbara och underhållbara skript genom att tillåta användning av meningsfulla strängnycklar istället för numeriska index.
