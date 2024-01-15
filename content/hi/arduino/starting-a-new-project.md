---
title:                "एक नया परियोजना शुरू करना"
html_title:           "Arduino: एक नया परियोजना शुरू करना"
simple_title:         "एक नया परियोजना शुरू करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Kyon
Kisi bhi naye project ko shuru karne ke liye ek bahut bada karan hai - uske through hum nayi cheezon ki khoj karte hain aur apne dimag ko chunauti dete hain. Arduino programming se shuru karna aapko apne computer se door khade devices tak pahunchne mein madad karega.

## Kaise Karein
Aapne socha hoga ki Arduino programming bahut mushkil ho sakti hai, par sach toh yeh hai ki yeh bahut hi asaan hai. Sabse pehle aapko apne computer mein Arduino software download karna padega. Phir, yeh steps follow karein:

```Arduino
void setup() {
  // Initialize the pins
  pinMode(LED_BUILTIN, OUTPUT);
}

void loop() {
  // Blinking LED
  digitalWrite(LED_BUILTIN, HIGH); // LED ON
  delay(1000); // Wait for a second
  digitalWrite(LED_BUILTIN, LOW); // LED OFF
  delay(1000); // Wait for a second
}
```

Is code mein humne LED ko ON-OFF karne ke liye ek blinking effect create kiya hai. Ab bas apne Arduino board ko apne computer se connect karein aur upar diye gaye code ko upload karein. Aap dekhenge ki LED blink ho rahi hai!

## Gehri Jhaank
Naye project shuru karte time, aapko ek clear idea hona chahiye ki aap kya banana chahte hain aur uska kya use hoga. Arduino programming aapko apne project ko step by step develop karna sikhata hai. Ismein aap sensors, motors, LEDs jaisi components ko connect kar sakte hain aur unhe control kar sakte hain.

## Dekhiye Bhi
- [Getting Started with Arduino](https://www.arduino.cc/en/Guide/Introduction)
- [Arduino Tutorials for Beginners](https://www.arduino.cc/en/Tutorial/BuiltInExamples)
- [Beginner's Guide to Arduino](https://www.makerspaces.com/arduino-guide/)

Dhanyavaad, aur sukriya ki aapne hamare article ko padha! Happy coding!