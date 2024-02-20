---
date: 2024-01-26 03:46:26.137527-07:00
description: "Att avrunda tal inneb\xE4r att justera dem f\xF6r att vara n\xE4rmare\
  \ ett enklare eller mer signifikant v\xE4rde. Programmerare avrundar tal f\xF6r\
  \ att f\xF6renkla\u2026"
lastmod: 2024-02-19 22:04:56.718902
model: gpt-4-0125-preview
summary: "Att avrunda tal inneb\xE4r att justera dem f\xF6r att vara n\xE4rmare ett\
  \ enklare eller mer signifikant v\xE4rde. Programmerare avrundar tal f\xF6r att\
  \ f\xF6renkla\u2026"
title: Avrundning av tal
---

{{< edit_this_page >}}

## Vad & Varför?
Att avrunda tal innebär att justera dem för att vara närmare ett enklare eller mer signifikant värde. Programmerare avrundar tal för att förenkla resultat, begränsa antalet decimaler för visning, eller för vissa matematiska ändamål.

## Hur man gör:
Här är det grundläggande om att avrunda tal i Python:

```python
# Avrunda ett tal till närmaste heltal
print(round(8.67))  # Ger: 9

# Avrunda ett tal till ett specificerat antal decimaler
print(round(8.67, 1))  # Ger: 8.7

# Jämna tal avrundas neråt och udda tal avrundas uppåt när de är lika långt ifrån
print(round(2.5))  # Ger: 2
print(round(3.5))  # Ger: 4
```

## Fördjupning
I Python är `round()` inte bara att hacka av decimaler. Historiskt sett följer Python, som många andra språk, "avrunda hälften till jämnt" eller "bankiravrundning". Detta minimerar kumulativt fel i summor eller medelvärden, vilket spelar roll i finansiella beräkningar.

För alternativ har du `math.floor()` och `math.ceil()` från Pythons matematikmodul, som drar ner eller upp tal till nästa hela tal. Men om det är precision du är ute efter, låter `decimal`-modulens `quantize()` dig specificera avrundningsbeteende.

Under huven hanterar `round()` binära flyttal. Eftersom vissa decimaler inte kan uttryckas exakt i binärt, kan du bli överraskad av saker som `round(2.675, 2)` inte blir `2.68` som förväntat. Här kommer `decimal` eller `fractions` in för hög precision.

## Se även
- Pythons dokumentation om inbyggda funktioner: https://docs.python.org/3/library/functions.html#round
- Decimal fast punkt- och flyttalsaritmetik: https://docs.python.org/3/library/decimal.html
- Pythons matematikmodul: https://docs.python.org/3/library/math.html
