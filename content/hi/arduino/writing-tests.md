---
title:                "लिखने के परीक्षण"
html_title:           "Arduino: लिखने के परीक्षण"
simple_title:         "लिखने के परीक्षण"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Tests likhne ka matlab hai ki hum apne code ko test karte hain. Programmers isliye ye tests likhte hain taaki unhe apne code mein kisi bhi prakar ki bug ya error ki jaach ho sake. Ye hamein apne code ki quality ko improve karne mein madad karte hain.

## How to:
Arduino mein tests likhna kaafi aasaan hai. Bas hume apne code ke sahi tarike se tests ki zaroorat hoti hai. Ye tests hum ```Arduino ... ``` code blocks mein likhte hain. Neeche kuch examples diye gaye hain, jo aapko samajhne mein madad karenge.

### Example 1:
```
void setup() {
  pinMode(13, OUTPUT);
}

void loop() {
  digitalWrite(13, HIGH);
  delay(1000);
  digitalWrite(13, LOW);
  delay(1000);
}
```

Is code mein humne LED ko blink karne ke liye test likha hai. ```digitalWrite``` function ke madad se hum LED ki state ko HIGH ya LOW kar sakte hain. Is code mein humne 1 second ke intervals mein LED ko ON-OFF kiya hai.

### Example 2:
```
int num1 = 10;
int num2 = 20;

int result = num1 + num2;

Serial.println(result);
```

Is code mein humne do integers ko add karne ka test likha hai. Fir us result ko ```Serial.println``` function se print kiya hai. Is tarah se hum apne code ko test kar sakte hain.

## Deep Dive:
Tests likhna ek behad zaroori practice hai. Isse hum apne code mein bugs ko identify kar sakte hain aur unhe fix kar sakte hain. Isse humari code ki quality improve hoti hai aur final product bhi reliable banta hai.

Tests likhne ke kuch alternatives hote hain, jaise ki manual testing ya automated testing. Manual testing mein hum khud hi apne code ko run karke check karte hain. Jabki automated testing mein humne pehle se hi code likha hota hai jo humare code ko test karta hai aur hume error ya bugs ki report deta hai.

Arduino mein code ko test karne ke liye hum ```assertEquals()``` function ka bhi istemaal kar sakte hain. Isse hume expected aur actual results ko compare karne mein madad milti hai.

## See Also:
- [Arduino Testing Library](https://github.com/mmurdoch/arduinounit)