---
title:                "डायरेक्टरी मौजूद है कि नहीं जांचना"
html_title:           "C: डायरेक्टरी मौजूद है कि नहीं जांचना"
simple_title:         "डायरेक्टरी मौजूद है कि नहीं जांचना"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Kyun

Kya aap kabhi apne program ke andar kisi directory ki existence ka pata lagana chahte hai? Yeh zaruri hai jab aap kisi file ko read ya write karna chahte hai aur uske liye woh directory exist hona chahiye. Is article mein hum dekhenge ke directory ki existence kaise check ki jaa sakti hai aur iska kya importance hai.

## Kaise Karein

```
C
#include <stdio.h>
#include <unistd.h>

int main() {
    char* path = "/home/user/example_dir";

    if (access(path, F_OK ) != -1 ) {
        printf("Directory exists.\n");
    } else {
        printf("Directory does not exist.\n");
    }

    return 0;
}
```
Output:
```
Directory exists.
```

Yahan humne C programming language mein ek code example diya hai. Ismein humne `access()` function ka use kiya hai jo `unistd.h` library mein available hai. Humne `access` function mein do parameters diye hai - path aur `F_OK` constant. Agar directory exist karti hai, toh hum `Directory exists` print karenge. Agar directory exist nahi karti, toh hum `Directory does not exist` print karenge. Is tarah se hum code mein `if` condition se directory ki existence check kar sakte hai.

## Deeper Info

Directory ki existence check karne ke liye hum `access()` ya `stat()` function ka use kar sakte hai. `access()` function mein hum `F_OK` ke saath aur bhi options jaise `R_OK` (read), `W_OK` (write), `X_OK` (execute) ka use kar sakte hai. Agar hum sirf directory ki existence check karna chahte hai, toh `F_OK` ka use karna sufficient hai.

## Dekhiye Bhi

- [Online C Compiler](https://www.onlinegdb.com/online_c_compiler)
- [GeeksforGeeks](https://www.geeksforgeeks.org/c-programming-language/)
- [The C Book](https://publications.gbdirect.co.uk/c_book/)