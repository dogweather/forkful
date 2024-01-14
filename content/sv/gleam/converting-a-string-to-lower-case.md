---
title:    "Gleam: Konvertera en sträng till gemener"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till små bokstäver är en vanlig operation inom programmering och kan vara användbar i många olika situationer. Till exempel kan det användas för att standardisera inmatade strängar och för att göra jämförelser mellan olika strängar mer exakta.

## Hur man gör det

För att konvertera en sträng till små bokstäver i Gleam använder man en inbyggd funktion som heter `String.to_lower`. Denna funktion tar in en sträng som argument och returnerar en ny sträng med alla bokstäver i små bokstäver.

```Gleam
let original_sträng = "HeJ!"

let låga_strängen = String.to_lower(original_sträng)

io.println(låga_strängen)
```

Output:

```
hej!
```

## Djupdykning

När man konverterar en sträng till små bokstäver är det viktigt att tänka på att olika språk och alfabet kan ha olika regler för stora och små bokstäver. Till exempel kan vissa språk använda accenttecken som också behöver tas hänsyn till vid konverteringen. Därför kan det vara bra att använda sig av funktionen `String.to_lower_case_normalized` istället, som också tar hänsyn till dessa skillnader.

## Se även

- [Gleam dokumentation för String modulen](https://gleam.run/documentation/stdlib/string/)
- [En guide för att arbeta med strängar i Gleam](https://medium.com/@gleamlang/guide-to-working-with-strings-in-gleam-4f7c0458d69f)