---
title:    "Gleam: Radera tecken som matchar ett mönster"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

Varför: Det kan finnas flera anledningar till varför man skulle vilja ta bort tecken som matchar ett visst mönster i en Gleam-kod. En av de vanligaste är för att rensa upp kod som inte längre används eller är föråldrad.

Hur man gör det: För att ta bort tecken som matchar ett visst mönster i en Gleam-kod, kan man använda funktionen "String.replace" och ange mönstret och ersättningsvärdet. Här är ett exempel på hur man kan göra det:

```Gleam
let orig_str = "Hej världen"
let new_str = String.replace(orig_str, ~pattern="världen", ~replacement="")
IO.println(new_str) // Utskrift: "Hej "
```

Djupdykning: Om du vill ta bort flera förekomster av ett visst mönster i en sträng, kan du använda funktionen "String.replace_all" istället. Detta kommer att ta bort alla förekomster av mönstret och ersätta dem med det angivna värdet. Här är ett exempel på hur man kan använda det:

```Gleam
let orig_str = "Hej kära världen, kära Gleam-utvecklare"
let new_str = String.replace_all(orig_str, ~pattern="kära", ~replacement="")
IO.println(new_str) // Utskrift: "Hej världen, Gleam-utvecklare"
```

Se även: För mer information om stränghantering i Gleam kan du kolla in Gleams officiella dokumentation (länk till: https://gleam.run/documentation/strings) och det här inlägget om hur man manipulerar strängar med hjälp av inbyggda funktioner (länk till: https://medium.com/@jlouis666/guide-to-manipulating-strings-in-gleam-eff0184d2b10).