---
title:                "Elixir: Omvandling av en sträng till gemener"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Det finns många tillfällen när du som programmerare kan behöva konvertera en sträng (engelska: string) till små bokstäver. Detta kan vara för att enhetligt formatera inmatade data, jämföra strängar utan att ta hänsyn till versaler eller för att undvika fel när du använder en funktion som kräver små bokstäver. Oavsett vad anledningen är, är det användbart att veta hur man på ett enkelt sätt kan göra detta i Elixir.

## Så här gör du

Konvertering av strängar till små bokstäver är väldigt enkelt i Elixir tack vare funktionen `String.downcase/1`. Denna funktion tar in en sträng som argument och returnerar en ny sträng med alla bokstäver i små bokstäver. 

```Elixir
iex> String.downcase("HEJ DET HÄR ÄR EN STRÄNG")
"hej det här är en sträng"
```

En annan sätt är att använda operatorn `<>`(engelska: string concatenation) tillsammans med `String.upcase/1` och `String.downcase/1` för att byta ut alla versaler till små bokstäver och sammanfoga dem med en tom sträng. 

```Elixir
iex> "Hej, " <> String.downcase("DET HÄR ÄR EN STRÄNG")
"Hej, det här är en sträng"
```

## Djupdykning

När Elixir bearbetar strängar använder det Unicode-tecken (engelska: Unicode) som standard. Detta betyder att det finns support för flera olika språk och teckenuppsättningar inbyggt i språket. När man konverterar en sträng till små bokstäver tar funktionen `String.downcase/1` hänsyn till denna språksupport och konverterar även eventuella specialtecken till små bokstäver. Detta är användbart om du behöver hantera flerspråkiga applikationer.

En annan användbar funktion för konvertering av strängar är `String.ascii_downcase/1` som endast konverterar bokstäverna a-z och A-Z till små bokstäver. Detta kan vara användbart vid jämförelser där du endast vill ta hänsyn till grundläggande engelska bokstäver.

## Se även

- Dokumentation för funktionen `String.downcase/1`: https://hexdocs.pm/elixir/String.html
- Utforskning av Unicode i Elixir: https://elixir-lang.org/getting-started/unicode-charlists-and-code-points.html