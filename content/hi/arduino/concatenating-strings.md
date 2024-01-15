---
title:                "स्ट्रिंग्स को जोड़ना"
html_title:           "Arduino: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Kyun
Aksar hume ek se adhik strings ko ek hi string me jodne ki zarurat hoti hai, jaise ki sensor ke readings ko display me dikhana ya messages ko bhejna. Iss liye hum concatenate strings ka use karte hain.

## Kaise Karein
```Arduino
String string1 = "Hello";
String string2 = "World!";
String result = string1 + " " + string2;
Serial.println(result);
```
Output:
```
Hello World!
```

Is code example me, humne do strings, "Hello" aur "World!" ko ek hi string me joda hai, jiska result "Hello World!" hua hai. Iss tarah se hum apni zaroorat ke hisab se strings ko jod sakte hain.

## Deep Dive
Concatenate strings ka use karne se pehle, humein `String` data type ka use karna hoga, jo ki ek string ko represent karta hai. Hum bhi `+` operator ka use karte hain, jo ki strings ko jod kar ek naya string bana deta hai.

Ek important point hai ki strings ko jodne se pehle, humein unhe convert karna padta hai `String` data type me. Iss tarah se hum strings aur variables ko bhi jod sakte hain.

## Dekhein Bhi
Humne iss article me strings ko concatenate karna sikh liya hai, lekin agar aapko aur complex operations aur functions ke bare me jaanna hai, toh humare listed resources check karein.

"See Also" 
- [Arduino String Concatenation](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/concat/)
- [Tutorial: String Operations in Arduino](https://howtomechatronics.com/tutorials/arduino/arduino-string-operations/)