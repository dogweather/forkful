---
title:    "PHP: Radera tecken som matchar ett mönster"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Varför
Att ta bort tecken som matchar ett visst mönster är en vanlig uppgift inom PHP-programmering och kan hjälpa till att rensa och strukturera data eller filtrera ut oönskad information.

## Så här gör du
För att ta bort tecken som matchar ett visst mönster i en sträng, kan du använda funktionen `preg_replace()` i PHP. Detta gör det enkelt att ersätta eller ta bort specifika tecken baserat på ett givet mönster.

```
<?php
$string = "Hello123 World!";
echo preg_replace("/[0-9]/","",$string); // Output: HelloWorld!
?>
```

I detta exempel använder vi `[0-9]` som mönster, vilket betyder att alla siffror från 0 till 9 kommer att tas bort från strängen och ersättas med ett tomt värde.

## Fördjupning
Det finns många olika sätt att använda `preg_replace()` för att ta bort tecken baserat på ett visst mönster. Här är några vanliga användningsområden:

- Ta bort alla bokstäver och behålla endast siffror: `preg_replace("/[A-Za-z]/","",$string)`
- Ta bort alla specialtecken: `preg_replace("/[^\w\s]/","",$string)`
- Ta bort alla mellanslag: `preg_replace("/\s/","",$string)`

Kom ihåg att du kan använda ett brett spektrum av mönster och kombinera dem för olika effekter. Du kan också använda olika funktioner, till exempel `preg_match()` för att hitta matchande tecken eller `preg_replace_callback()` för att ersätta tecken med din egen anpassade funktion.

## Se också
- [PHP: preg_replace - Manual](https://www.php.net/manual/en/function.preg-replace.php)
- [Regular Expressions - Tizag Tutorials](https://www.tizag.com/regexTutorial/)