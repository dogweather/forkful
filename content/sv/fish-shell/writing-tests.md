---
title:                "Skriva tester"
date:                  2024-01-19
simple_title:         "Skriva tester"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Tester ser till att din kod funkar som den ska. De förebygger fel och sparar tid under utveckling och underhåll.

## Hur till:
För att skriva och köra tester i Fish Shell, kan vi använda `fisher` för att installera ramverket `Fishtape`. Här är ett exempel:

```Fish Shell
fisher install jorgebucaran/fishtape

# Testfil: example_test.fish
function test_addition -S
    echo $((2 + 2)) | grep -q '4'
    and return 0
    or return 1
end

# Kör testet
fishtape example_test.fish
```

Förväntad utskrift:
```
TAP version 13
ok 1 test_addition
1..1
```

## Djupdykning
Fishtape startades 2015 och är inspirerat av TAP (Test Anything Protocol). Alternativ för Fish-skripttestning är begränsade jämfört med andra skal. Tester körs normalt på isolerade miljöer för att inte påverka din nuvarande session.

## Se Även
- Fish Shell officiella websida: [https://fishshell.com](https://fishshell.com)
- Fishtape på GitHub: [https://github.com/jorgebucaran/fishtape](https://github.com/jorgebucaran/fishtape)
- `fisher` plugin manager: [https://github.com/jorgebucaran/fisher](https://github.com/jorgebucaran/fisher)
