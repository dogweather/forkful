---
title:                "स्ट्रिंग को लोअर केस में रूपांतरित करना"
html_title:           "C: स्ट्रिंग को लोअर केस में रूपांतरित करना"
simple_title:         "स्ट्रिंग को लोअर केस में रूपांतरित करना"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Kyun
Yadi aap ek programmer hain aur ek string ko lower case mein convert karna sikhna chahte hain, toh ye article aapke liye hai. Lower case strings bahut kaam aati hain, jaise ki user input validation aur searching algorithms mein.

## Kaise Kare
Aap C programming language mein string ko lower case mein convert karne ke liye kuch steps follow kar sakte hain. Sabse pehle, hum ek character array declare karenge jismein hum string ko store karenge. Phir hum loop ka use karke har character ko check karenge aur usko lower case mein convert karenge. Neeche diye gaye code blocks mein aap isko dekh sakte hain.

```C
#include<stdio.h>
#include<string.h>

int main(){
    // declaring character array
    char str[50];

    // taking user input
    printf("Enter a string: ");
    scanf("%s", str);

    // loop to convert characters to lower case
    for(int i=0; i<strlen(str); i++){
        if(str[i] >= 'A' && str[i] <= 'Z'){
            str[i] = tolower(str[i]);
        }
    }

    // printing the lower case string
    printf("Lower Case String: %s", str);

    return 0;
}
```

Output:
```C
Enter a string: HELLO WORLD
Lower Case String: hello world
```

## Gehraai Mein Jaaenge
String ko lower case mein convert karne ke liye hum ASCII values ka use karte hain. Har character ko ASCII value ke saath compare karke hume pata chalta hai ki wo upper case ya lower case hai. ASCII values 65 se 90 tak uppercase characters ko represent karte hain, 97 se 122 tak lowercase characters ko represent karte hain. Isi ko humne loop mein use kiya hai.

## Dekhiye Bhi
[GeeksforGeeks article on converting strings to lower case in C](https://www.geeksforgeeks.org/convert-string-lower-case-ascii-value/)

[C programming language tutorial](https://www.programiz.com/c-programming)

[ASCII table for reference](https://www.asciitable.com/)