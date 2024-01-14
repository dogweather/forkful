---
title:                "Arduino: क्या एक निर्देशिका मौजूद है की जांच"
simple_title:         "क्या एक निर्देशिका मौजूद है की जांच"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Kyon

Arduino ke sath programming karne ke dauran, kabhi kabhi aapko check karna padega ki kya ek directory hai ya nahi. Directory, ya folder, humare code ke liye important hota hai kyunki isme hum apne files ko organize karte hain. Isliye, ek directory ki upasthiti ko check karna zaroori hai taaki hum sahi tarah se apne files ko access kar sakein.

## Kaise Karein

```Arduino
#include <SD.h> // library ko import karein

if (SD.exists("/folder")) { // directory ki upasthiti ko check karein
  Serial.println("Directory exists!"); // agar directory maujood hai toh serial monitor par message print karein
}
```
Output:
```
Directory exists!
```

Is coding example mein humne `SD.exists()` function ka upyog kiya hai, jo ek boolean value return karta hai, yaani true ya false. Yadi directory maujood hai toh true return hoga aur hum message print karenge, warna false return hoga aur kuch nahi hoga.

Hum bhi `SD.mkdir()` function ka upyog karke ek naya directory create kar sakte hain, jaisa ki neeche coding example mein dikhaya gaya hai:

```Arduino
#include <SD.h> // library ko import karein

if (!SD.exists("/folder")) { // agar directory nahi hai toh
  if (SD.mkdir("/folder")) { // directory create karein
    Serial.println("Directory created!"); // aur message print karein
  }
}
```
Output:
```
Directory created!
```

## Gehri Khudaai

Directory ki upasthiti ko check karna kai situations mein zaroori ho sakta hai. Jaise, agar hume kisi specific file ko read ya write karna hai toh hume pahle directory ki upasthiti ko check karna padega. Iske alawa, hum bhi `SD.remove()` function ka upyog karke directory ko delete kar sakte hain. Is tarah se, directory ki upasthiti ko check karna bahut zaroori ho sakta hai humare Arduino project ke liye.

## Dekhiye Bhi

Agar aapko aur jaankari chahiye directory ko check karne aur manipulate karne ke baare mein, toh neeche diye gaye resources ko dekhein:

- [Arduino SD library reference](https://www.arduino.cc/en/Reference/SD)
- [How to create and delete folders with Arduino SD library](https://randomnerdtutorials.com/arduino-sd-card-how-to-create-and-delete-files/)
- [Beginner's guide to Arduino](https://learn.sparkfun.com/tutorials/sik-experiment-guide-for-arduino---v32) (Hindi translation available on the site)