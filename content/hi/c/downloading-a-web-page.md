---
title:                "एक वेब पेज डाउनलोड करना"
html_title:           "C: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Kyon

Jab apne pass ek acchi computer aur internet connection ho, tab aap kai baar apne browser mein kisi web page ko open karte hai. Par kya aapne kabhi socha hai ki ye web page aapke computer par kaise load hota hai? Yeh possible hai kyunki hum C programming language ka use karke web page ko download kar sakte hai. Aaj hum dekhenge ki kaise hum C ke madad se ek web page ko download kar sakte hai.

## Kaise Kare

Sabse pehle, humein ek C compiler ki jarurat hogi jaise ki Turbo C ya GCC. Agar aapke paas ye nahi hai toh aap ise download kar sakte hai. Fir hum ```#include <stdio.h>``` aur ```#include <string.h>``` use karke required libraries ko load karenge. Iske baad hum ek function banayenge jo ek web page ko download karega, jaise ki ```void download_page(char url[])```.

Fir hum ye function call karenge aur usmein ek URL bhi pass karenge jis web page ko hum download karna chahte hai. Iske baad hum ek file ko open karenge jisme hum web page ko save karenge. Ab hum web page ka content download karne ke liye ek loop chalayenge aur use file mein write karenge. Yehi process hota hai jab hum kisi bhi file ko download karte hai, bas fark itna hai ki hum webpage ke URL par request send karte hai. Jab humari loop puri ho jayegi, hum file ko close kar denge aur web page download ho jayega. Niche diye gaye code snippet se aap is process ko acchi tarah se samajh sakte hai.

```C
#include<stdio.h>
#include<string.h>

void download_page(char url[]) {
   FILE *file = fopen("downloaded_page.html", "w"); // file ko write mode mein kholiye
   char buffer[255];
   FILE *pipe = popen(url, "r"); // request send karne ke liye
   while(fgets(buffer, 255, pipe) != NULL) { // webpage ka content download karne ke liye loop chalaye
      fprintf(file, "%s", buffer); // file mein content write kare
   }

   fclose(file); // file ko close kare
   pclose(pipe); // pipe ko close kare
}

int main() {
   char url[255] = "https://example.com"; // yaha URL change kare apne choice ke hisab se
   download_page(url); // download_page function ko call kare
   return(0);
}
```

Output:

Ek baar code run hone ke baad, aapko apne current directory mein ek file "downloaded_page.html" ki tarah dikhengi. Aap ise notepad ya kisi bhi HTML editor mein open karke dekh sakte hai.

## Deep Dive

Jab hum C programming language ka use karke ek web page ko download karte hai, tab hum ek file jisme web page ka content save hota hai, bana lete hai. Hum is file ka use kisi bhi tarah se kar sakte hai. Jaise ki hum is file ka content parse karke, acche se format kar sakte hai aur kisi dusre file mein store kar sakte hai. Is tarah se hum web scraping ya data extraction bhi kar sakte hai.

See Also:

- https://www.geeksforgeeks.org/downloading-a-web-page-using-curl-library/

- https://www.codespeedy.com/download-web-page-source-data-using-c-programming-language/