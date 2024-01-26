---
title:                "Avrundning av tal"
date:                  2024-01-26T03:44:48.156610-07:00
model:                 gpt-4-0125-preview
simple_title:         "Avrundning av tal"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/rounding-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att avrunda tal handlar om att justera ett värde till den närmaste angivna enheten—som 2,56 till 3 om vi avrundar till hela tal. Programmerare gör detta för enkelhets skull eller för att uppfylla vissa numeriska specifikationer, vanligtvis för att undvika nyanser orsakade av fel i precisionen hos flyttal eller för att göra utdata mer användarvänlig.

## Hur man gör:
I Gleam finns inte avrundning i standardbiblioteket vid min senaste kontroll, men så här skulle du typiskt avrunda ett flyttal till det närmaste hela talet med hjälp av Erlangs funktioner direkt:

```gleam
external fn erlang_round(Float) -> Int = "erlang" "round"

pub fn main() {
  let rounded = erlang_round(2.56)
  rounded // Utskrift: 3
}
```

Utskrift:
```
3
```

Har du en annan precision i åtanke? Säg, avrundning till två decimaler? Vi behöver lite matematik:

```gleam
pub fn round_to_two_places(num: Float) -> Float {
  let multiplier = 100.0
  let tmp = num * multiplier
  let round_tmp = erlang_round(tmp)
  round_tmp / multiplier
}

pub fn main() {
    round_to_two_places(2.569) // Utskrift: 2.57
}
```

Utskrift:
```
2.57
```

## Djupdykning
Historiskt sett har avrundning av tal varit avgörande, särskilt inom finansiella och vetenskapliga beräkningar där precision och standarder spelar en stor roll. Utan avrundning skulle du få långa, ohanterliga decimaltal överallt, vilket skulle göra beräkningar opraktiska och felbenägna.

I programmeringsvärlden erbjuder olika språk olika tillvägagångssätt, från inbyggda funktioner till omfattande matematikbibliotek. Avrundning kan innebära olika regler - till exempel "avrunda uppåt" (den vanliga metoden) eller "avrunda till jämnt tal" (ofta använt i finansiella beräkningar för att undvika partiskhet).

Gleam, som är ett ungt språk med rötter i Erlang, förlitar sig på Erlangs robusta uppsättning numeriska funktioner. När språket växer kan vi få se inbyggda funktioner introduceras, vilket minskar behovet av att anropa externa rutiner.

## Se även
- Erlangs :math modul för fler nummerkrunchande: https://erlang.org/doc/man/math.html
- För bakgrund om varför avrundning kan bli knepigt, IEEE:s standard för flyttal: https://ieeexplore.ieee.org/document/8766229
- Intresserad av matematiken bakom detta? Kolla in "Vad varje datavetare bör veta om flyttalsaritmetik": https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html