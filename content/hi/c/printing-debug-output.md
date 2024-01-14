---
title:                "C: डिबग आउटपुट प्रिंट करना"
programming_language: "C"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Kyun

Kabhi kabhi hum apne program mein kuch gadbad ho jaati hai aur humme pata nahi hota ki kya chal raha hai. Iske liye, hum debug output ka istemaal karte hain. Debug output ki madad se hum apne program ke har step ko dekh sakte hain aur kisi issue ko identify kar sakte hain. Isse humme apne program ko sahi tarike se chalane mein madad milti hai. 

## Kaise Karein

Debug output ka istemaal karne ke liye, hum `printf` function ka upyog karte hain. Is function ka istemaal humme apne program mein kuch values ya variables ko print karne ki anumati deta hai. Yahan hum ek simple example dekhenge:

```C
#include <stdio.h> 
int main() 
{ 
    int num1 = 5;
    int num2 = 10;
    printf("The value of num1 is %d\n", num1);
    printf("The value of num2 is %d\n", num2);
    return 0; 
} 
```
Is code mein humne `printf` function ka istemaal kiya hai aur `num1` aur `num2` variables ke values ko print kiya hai. Is tarah se hum apne program ke different sections mein output ko dekh sakte hain aur kisi issue ko identify kar sakte hain.

## Gehraai Mein Jaaen

Debug output ka istemaal karna ek bahut hi useful technique hai jisme hum apne program ko aur bhi gehraai se samajh sakte hain. Hum apne program mein `printf` statement ko multiple baar use kar sakte hain aur different variables aur values ko compare kar sakte hain. Isse humme kisi specific issue ko pinpoint karne mein madad milti hai.

Iske alawa, hum apne program mein `#ifdef` aur `#ifndef` pre-processor directives ka istemaal karke bhi debug output enable aur disable kar sakte hain. Agar hum debug output ko disable karna chahte hain, toh hum `#ifdef` directive ka upyog karte hain, aur agar enable karna chahte hain toh `#ifndef` ka upyog karte hain.

## Dekhen Bhi

Ab aap jaan chuke hain ki debug output kya hai aur kaise aap iska upyog kar sakte hain. Agar aap aur gehraai se jaana chahte hain toh neeche diye gaye articles ko padh sakte hain:

- [Difference between printf and fprintf in C programming](https://www.geeksforgeeks.org/difference-printf-fprintf-c-language/)
- [A Visual Explanation of C Used in a Printf Statement](https://www.youtube.com/watch?v=1jXNAGogKqI)

Dhanyawaad!