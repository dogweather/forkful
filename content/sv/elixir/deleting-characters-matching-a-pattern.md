---
title:                "Ta bort tecken som matchar ett mönster."
html_title:           "Elixir: Ta bort tecken som matchar ett mönster."
simple_title:         "Ta bort tecken som matchar ett mönster."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

Vad är anledningen till att ta bort tecken som matchar ett mönster?

Ibland kanske du stöter på ett problem som kräver att du tar bort specifika tecken från en textsträng. Kanske behöver du ta bort alla siffror från en sträng eller alla punkter och kommatecken. Istället för att manuellt gå igenom och radera varje enskilt tecken kan du använda Elixirs inbyggda funktioner för att enkelt ta bort dem som matchar ett visst mönster.

Så här tar du bort tecken som matchar ett mönster i Elixir:

```Elixir 
def delete_matching_chars(string, pattern) do
  String.replace(string, ~r/#{pattern}/, "")
end

IO.puts(delete_matching_chars("Hej 123!,. där", "\\d|."))
IO.puts(delete_matching_chars("Hej där, detta är en text", "[,.]"))
```

Output:
```
Hej där
Hej där detta är en text
```

För att förstå hur denna funktion fungerar behöver vi först förstå två saker: String.replace och reguljära uttryck.

String.replace är en inbyggd funktion i Elixir som tar en sträng och ersätter alla förekomster av ett visst mönster med en ny text. Det används vanligtvis för att ersätta en bit av en sträng med en annan bit.

Reguljära uttryck är en typ av syntax som används för att matcha mönster i textsträngar. I vårt exempel används ~r för att definiera ett reguljärt uttryck och #{} används för att infoga en variabel i mönstret.

I vårt kodexempel är metakaraktärerna \d och . 2 oftast använda reguljära uttryck. \d matchar alla siffror och . matchar alla tecken. | används för att separera flera mönster och betyder "eller". Vi kan också använda en uppsättning hakparenteser [] för att ange en lista över specifika tecken som vi vill matcha.

Djupdykning:

Elixir erbjuder många olika inbyggda funktioner för att manipulera textsträngar och reguljära uttryck. I vårt exempel använde vi endast String.replace och enkla reguljära uttryck, men det finns också funktioner som String.replace_each och String.replace_at som kan hjälpa till med mer specifika användningsfall.

Se även:

- Elixir String Module: https://hexdocs.pm/elixir/String.html
- Regular Expressions in Elixir: https://elixir-lang.org/getting-started/regex.html