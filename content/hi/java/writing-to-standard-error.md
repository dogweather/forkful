---
title:                "मानक त्रुटि पर लेखन"
html_title:           "Java: मानक त्रुटि पर लेखन"
simple_title:         "मानक त्रुटि पर लेखन"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Kyon
Kya aapne kabhi console par code likhte waqt kisi galat input ko handle karne ki kosish ki hai? Ya fir aapke program mein koi error aa gaya aur aapko use debug karna hai? Yeh sab situations mein aapko standard error ki madad leni padti hai. Standard error, ya stderr, coding mein ek bahut important concept hai jo aapki debugging aur error handling ko asaan banata hai. Is article mein hum dekhenge ki Java mein standard error ko kaise likha jata hai aur iska humare coding process mein kya mahatva hai.

## Kaise Karein
Standard error ko likhne ke liye, `System.err.println()` ka use kiya jaata hai. Iske saath saath, exceptions aur errors ko handle karne ke liye `try-catch` blocks ka bhi use kiya jaata hai. Yeh humne niche diye gaye example mein dikhaya hai:

```Java
import java.io.*;

public class StandardErrorExample{
  public static void main(String[] args){
    try{
      // kuch code jo error throw kar sakta hai
      // jaise ki: Integer.parseInt("abc") 
    }catch(NumberFormatException e){
      // yahan par error message ko standard error par print kiya jata hai
      System.err.println("Invalid input! Please enter a valid integer.");
    }
  }
}
```

Is code mein, agar kisi user ne `abc` ko integer mein convert karne ki koshish ki, toh `NumberFormatException` exception throw hoga. Is exception ko handle karne ke liye `try-catch` block ka use kiya jaata hai aur uske andar humne standard error par ek error message print kiya hai. Is tarah se hum apne program mein errors ko handle kar sakte hain aur debugging process ko asaan bana sakte hain.

## Gehre Vistar
Standard error ka term shayad aapne pehle kabhi nahi suna hoga. Iske saath saath, yeh concept shuruwat mein confusing bhi lag sakta hai. Lekin jab hum iska use karein, hume uski importance aur functionality samajh mein aati hai. Yeh hume errors ko handle karne aur debugging karne mein madad karta hai.

Ek interesting fact yeh hai ki standard error, apne naam ke khilaaf, ek error nahi hai. Yeh ek stream hai jo error messages aur debugging information ko console par print karta hai. Isse humara console output bhi organized aur clear rehta hai.

Is article mein humne `System.err.println()` ka use dekha. Lekin, Java mein aur bhi bahut saare classes aur methods hain jo standard error ko handle karte hain aur hume iske saath aur bhi kaafi depth tak jaane ki madad karte hain. Agar aap coding mein mastery chahte hain, toh standard error ka concept zaroor samajhna hoga.

## Dekhiye Bhi
- [Java Exceptions and Error Handling](https://www.w3schools.com/java/java_exceptions.asp)
- [Understanding Java Exceptions](https://www.geeksforgeeks.org/understanding-exceptions-in-java/)
- [Standard Error vs Standard Output in Java](https://www.baeldung.com/java-printstream-printf-println-println)