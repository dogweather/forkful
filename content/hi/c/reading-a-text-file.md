---
title:    "C: टेक्स्ट फाइल पढ़ना"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Kyu
Agar aap ek C programmer hai aur aapko text files ko padhna aur samajhna zaroori hai, toh is blog post ko padhna aapke liye faaydemand ho sakta hai.

## Kaise Kare
Agar aap text file ko C program me padhna chahte hai, toh iss kaam ke liye hum `fopen()` aur `fscanf()` functions ka istemal karenge. Yeh functions file ko open aur read karne me madad karte hai. Neeche diye gaye code blocks me aap dekh sakte hai ki kaise hum in functions ka istemal kar sakte hai:


```C
// File ko open karte hai
FILE *myFile = fopen("myfile.txt", "r");

// Agar file successfully open hui hai
if(myFile!=NULL){
    char str[100];

    // File se data read karte hai
    fscanf(myFile, "%s", &str);  
    
    // Output ko print karte hai
    printf("%s", str);  
    
    // File ko close kar dete hai
    fclose(myFile);  
}
```

Yahan humne `myfile.txt` file ko read kar ke usme se ek string ko read kiya aur use output me print kiya. Aap is code ko apni zaroorat ke hisaab se modify karke use kar sakte hai.

## Gehraai Me Jaane
Text file ko padhna aur samajhna koi mushkil kaam nahi hai. Aapko bas `fopen()` aur `fscanf()` functions ka istemal karna hoga aur aap file se data read kar sakte hai. Aap file ki type bhi specify kar sakte hai, jaise `.txt`, `.csv`, `.dat` etc. Aap chahe toh multiple files ko bhi ek sath read kar sakte hai.

Iss post se aapko ek basic idea mil gaya hoga ki kaise hum C program me text files ko read kar sakte hai. Agar aapko aur gehraai me jaana hai toh aap online tutorials aur resources se aur jankari prapt kar sakte hai.

## Dekhiye Bhi
Iss blog post se related aur helpful resources ke liye neeche diye gaye links ko check kare:

- [GeeksforGeeks - Reading and Writing to a Text File in C](https://www.geeksforgeeks.org/fopen-for-an-existing-file-in-write-mode/)
- [C Tutorials - File Handling](https://www.cprogramming.com/tutorial/cfileio.html)
- [Programiz - File Handling in C](https://www.programiz.com/c-programming/c-file-input-output)
- [Tutorialspoint - Handling Text Files in C](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)

Ab aap text files ko C program me read karne ke bare me puri tarah se samajh gaye honge. Happy coding!