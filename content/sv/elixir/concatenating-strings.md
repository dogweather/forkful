---
title:                "Elixir: Sammanslagning av strängar"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

# Varför
Att konkatenera strängar, det vill säga sammanslå flera strängar till en enda, är en grundläggande och användbar funktion inom Elixir-programmering. Genom att kunna sammanslå strängar kan du skapa dynamiska texter och formatera data på ett smidigt sätt.

## Hur man gör
Att konkatenera strängar i Elixir är enkelt. Använd funktionen `<>` för att sammanslå två eller flera strängar. Se nedan för exempel:

```Elixir
sträng1 = "Hej "
sträng2 = "Elixir"
konkatenerad_sträng = sträng1 <> sträng2
IO.puts konkatenerad_sträng
```

Output:
```Elixir
Hej Elixir
```

Du kan även konkatenera strängar med andra värden, som till exempel en variabel eller en konstant. Se exempel nedan:

```Elixir
namn = "Anna"
ålder = 28
IO.puts "Mitt namn är " <> namn <> " och jag är " <> ålder <> " år gammal."
```

Output:
```Elixir
Mitt namn är Anna och jag är 28 år gammal.
```

## Djupdykning
När du konkatenerar strängar i Elixir skapas en helt ny sträng istället för att ändra på de ursprungliga strängarna. Detta sker på grund av Elixir's immutabilitet, vilket innebär att värden inte kan ändras efter att de har skapats. Detta kan kallas för "copy and concat" approach.

En annan viktig aspekt är att funktionen `<>` tar två argument av samma typ, vilket innebär att om du försöker sammanslå en sträng med en annan typ av värde, som en integer eller en lista, kommer det att resultera i ett error.

## Se även
För mer information om strängar och andra grundläggande koncept inom Elixir, kolla in följande resurser:

- [Elixir School](https://elixirschool.com/sv/)
- [Officiell Elixir Dokumentation](https://hexdocs.pm/elixir/overview.html)
- [Awesome Elixir](https://github.com/h4cc/awesome-elixir) - en samling av användbara bibliotek, tools och resurser för Elixir-programmering.