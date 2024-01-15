---
title:                "Sökning och ersättning av text"
html_title:           "Arduino: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför
När du skriver kod för ditt Arduino-projekt är det lätt att göra misstag och behöva ändra text på flera ställen. Genom att använda sök- och ersättfunktionen kan du snabbt och enkelt ändra alla förekomster av en viss text i ditt kod.

## Så här gör du

För att söka och ersätta text i din Arduino-kod, används funktionen ```replace``` tillsammans med ```find```. Här är ett exempel på hur du kan använda dem:

```
Arduino.replace("fel", "rätt");
Arduino.find("fel");
```

Det här koden kommer att hitta alla förekomster av ordet "fel" och ersätta dem med ordet "rätt". Om du vill söka efter flera ord på en gång kan du använda en lista med ord istället för bara ett ord:

```
Arduino.replace(["fel1", "fel2", "fel3"], "rätt");
Arduino.find(["fel1", "fel2", "fel3"]);
```

Du kan också använda en variabel för att söka efter ett visst ord:

```
String fel = "fel1";
Arduino.replace(fel, "rätt");
Arduino.find(fel);
```

När du använder funktionen ```replace``` och ```find```, så är det viktigt att du håller koll på stor- och småbokstäver. Om du vill ersätta både "fel" och "Fel" måste du anropa funktionerna två gånger och ersätta båda varianterna av ordet.

För att se hur din kod kommer att se ut efter att du har använt funktionen ```replace```, kan du använda funktionen ```println```:

```
Arduino.println();
```

Det här kommer att skriva ut din kod med alla förekomster av det sökta ordet ersatt med det nya ordet.

## Deep Dive

När du använder dig av funktionerna ```find``` och ```replace``` på din kod, så kommer det att bli en högre bearbetning för Arduino, vilket kan påverka din kod för långsam. Om du märker att din kod tar väldigt lång tid att bli exekverad eller inte fungerar som den ska, kan du överväga att använda en annan sökmetod, som till exempel manuellt ersätta varje förekomst av den sökta texten.

## Se även

* [Funktionsreferens för Arduino - replace()](https://www.arduino.cc/reference/en/language/structure/strings/stringobject/replace/)
* [Funktionsreferens för Arduino - find()](https://www.arduino.cc/reference/en/language/functions/advanced-io/find/)