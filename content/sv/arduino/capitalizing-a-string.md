---
title:                "Arduino: Kapitalisering av en sträng"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##Varför

I den här bloggposten kommer vi att titta på hur du kan använda Arduino för att omvandla en sträng till versaler. Detta kan vara bra för många olika projekt, inklusive skyltar, namnbrickor eller andra liknande applikationer.

##Såhär gör du

Det finns flera olika sätt att omvandla en sträng till versaler med hjälp av Arduino. Ett enkelt sätt är att använda den inbyggda funktionen `toUpperCase()` som finns tillgänglig för strängar. Här är ett exempel på kod som visar hur du kan använda denna funktion:

```Arduino
String str = "Hej Världen!";
String strVersaler = str.toUpperCase();
Serial.println(strVersaler);
```

Detta kommer att skriva ut "HEJ VÄRLDEN!" på seriellmonitorn.

En annan metod är att använda en loop för att gå igenom varje tecken i strängen och omvandla det till versaler. Här är ett exempel på kod som gör detta:

```Arduino
String str = "Hej Världen!";
String strVersaler = "";

for (int i = 0; i < str.length(); i++) {
  strVersaler += toupper(str.charAt(i));
}
Serial.println(strVersaler);
```

Detta kommer att åstadkomma samma resultat som det första exemplet.

##Djupdykning 

För de som är intresserade av hur detta fungerar kan vi titta lite djupare på den inbyggda funktionen `toUpperCase()` som användes i det första exemplet. Funktionen är en del av objektet `String` och kallas med ett punkt mellanrum efter strängen som ska omvandlas. Funktionen använder sig av ASCII-koden för att identifiera och omvandla varje tecken till versaler.

I det andra exemplet används en loop för att iterera genom varje tecken i strängen. Inuti loopen används `toupper()` som är en inbyggd funktion i Arduino som omvandlar ett tecken till versaler baserat på dess ASCII-värde. Detta läggs sedan till i en ny sträng och skrivs ut när loopen är klar.

##Se även

- [Arduino Reference: String toUpperCase()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/touppercase/)
- [ASCII Table](https://www.ascii-code.com/)