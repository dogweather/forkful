---
title:                "C: कम्प्यूटर प्रोग्रामिंग पर एक लेख: कमांड लाइन आर्ग्यूमेंट पढ़ना"
simple_title:         "कम्प्यूटर प्रोग्रामिंग पर एक लेख: कमांड लाइन आर्ग्यूमेंट पढ़ना"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Kyun

Agar aap C programming ki duniya mei naye hai, toh shayad aapne ab tak command line arguments ke bare mei suna nahi hoga. Lekin ye ek bahut hi important concept hai jo aapko ek ache coder banne mei madad karega. Isiliye, agar aap hamare sath hai aur C programming mei kuch aage badhna chahte hai toh ye article aapke liye bahut helpful hoga!

# Kaise

Sabse pehle hume ye samajhna hoga ki command line arguments kya hote hai. Jab hum koi program banate hai toh hum usme kisi bhi input ko liya jata hai. Lekin command line arguments ek aise tareeke hai jisme hum program ko execute karte samay input de sakte hai. Isse hume program execute karne ke baad us input ka output milta hai.

Lekin ye kaam karne ke liye hume kuch steps follow karna hoga. Sabse pehle hume "main" function ko `argc` aur `argv` parameters ke sath define karna hoga. `argc` parameter hume batata hai ki kitne arguments humne program ko execute karte samay diye hai. Aur `argv` parameter ek array hai jo hume diye gaye arguments ko store karti hai.

Iske baad hum `for` loop ka use karke `argv` array mei se hume diye gaye arguments ko ek ek karke print karna hoga. Mai ek simple example ke sath samjhata hu:

```
#include <stdio.h>
int main(int argc, char *argv[])
{
  int i;
  printf("Program name: %s\n", argv[0]);
  if(argc == 1) //check if no arguments are given
  {
    printf("No arguments given\n");
  }
  else //print all arguments one by one
  {
    for(i = 1; i < argc; i++)
    {
      printf("Argument %d: %s\n", i, argv[i]);
    }
  }
  return 0;
}
```

Input:

```
command_line_arguments.exe Hello World!
```

Output:

```
Program name: command_line_arguments.exe
Argument 1: Hello
Argument 2: World!
```

# Deep Dive

Command line arguments ka use karke hum program ko command line se interact karne ke liye bhi use kar sakte hai. Jaise ki hum `argc` ka use karke ye check kar sakte hai ki koi argument input ke sath diya gaya hai ya nahi. Agar nahi diya gaya hai toh hum default value ka use kar sakte hai.

Iske alava bhi hum `argv` array mei diye gaye arguments ka index number aur value se access kar sakte hai. Ye kaam karne ke liye hum `*argv[]` syntax ka use karte hai.

# Dekhiye

Yadi aapko aur bhi detail mei command line arguments ke bare mei padhna hai toh neeche diye gaye links aapko madad karenge:

- https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp
- https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm
- https://www.programiz.com/c-programming/c-command-line-arguments