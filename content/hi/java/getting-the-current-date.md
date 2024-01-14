---
title:    "Java: वर्तमान तिथि प्राप्त करना"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Kyun

* Yadi aap ek Java programmer hai aur aapko apne code mein current date ka use karna hai, to aapko current date ko kaise obtain karna hai sikhna bahut zaroori hai. Yeh aapke code ko advanced aur accurate banayega.

# Kaise Karein

```Java
// Java mein current date ko obtain karna bahut hi aasaan hai. Hum SimpleDateFormat class ka use karke isey kar sakte hain.
// Sabse pehle hum SimpleDateFormat class ka object create karenge.
SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");

// Phir hum current date ko obtain karenge.
Date currentDate = new Date();

// Ab hum SimpleDateFormat class ke format method se current date ko humare desired format mein convert karenge.
String formattedDate = dateFormat.format(currentDate);

// Aur aakhri mein, hum yeh formatted date print kar sakte hain.
System.out.println(formattedDate);
```

**Output: 27/11/2021**

# Gahrai Mein Jaayein

Java mein current date ko obtain karna kaafi simple hai, lekin iskey peeche ka logic samajhna bhi zaroori hai. Yeh understanding aapko future mein apne code ko customize aur improve karne mein madad karegi.

Sabse pehle, hum SimpleDateFormat class ka use karte hain kyunki yeh date ko humare desired format mein convert karne ka functionality provide karta hai. Is class mein humein ek pattern specify karna hota hai, jiske according yeh date ko format karta hai.

Phir hum Date class ka use karke actual current date ko obtain karte hain. Yeh class humein current system date aur time ko return karta hai.

Aur aakhri mein, hum SimpleDateFormat class ka format method use karke current date ko humare desired format mein convert karte hain. Yeh formatted date string ko return karta hai.

# Dekhein Bhi

* [Java SimpleDateFormat Class Documentation](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
* [Java Date Class Documentation](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)