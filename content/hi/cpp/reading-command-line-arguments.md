---
title:                "कम्प्यूटर प्रोग्रामिंग पर लेख: कमांड लाइन तर्कों को पढ़ना"
html_title:           "C++: कम्प्यूटर प्रोग्रामिंग पर लेख: कमांड लाइन तर्कों को पढ़ना"
simple_title:         "कम्प्यूटर प्रोग्रामिंग पर लेख: कमांड लाइन तर्कों को पढ़ना"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Kyun

Kya aap jaante hain ki aap command line arguments kyun padhna chahte hain? Yah bahut zaroori hai jab hum kisi C++ program mein user se input lena chahte hain. Command line arguments humare program ko interact karne aur customize karne ka ek behtar tarika hai.

## Kaise Karein

Ab hum dekhenge ki hum command line arguments ko C++ mein kaise padh sakte hain.

Sabse pehle hum apne program mein "main" function ke arguments ko "argc" aur "argv" naam se declare karenge.

```C++
int main(int argc, char* argv[]) {
    // code
}
```

"argc" hume batayega ki kitne arguments humare program ko diye gaye hain aur "argv" hume un arguments ko store karne ke liye ek pointer array provide karega.

Ab hum "argv" array ke through arguments ko read aur use kar sakte hain. Chaliye ek example dekhein:

```C++
#include <iostream>

using namespace std;

int main(int argc, char* argv[]) {
    // arguments ko access karne ke liye loop chalate hain
    for (int i = 0; i < argc; i++) {
        cout << "Argument " << i << " is: " << argv[i] << endl;
    }
    return 0;
}
```

Is code mein humne "cout" statement ke through "argv" array ke elements ko print kiya hai. Yeh program run karne ke liye terminal mein arguments ke saath "program_name" bhi deni padegi. Is example mein humne 3 arguments diye hain: "Hello", "World" aur "123". Output is tarah hoga:

```
Argument 0 is: program_name
Argument 1 is: Hello
Argument 2 is: World
Argument 3 is: 123
```

## Deep Dive

Command line arguments ko padhne ka ek aur tareeka hai using "getopt" function. Yah ek library function hai jo hume option flags aur values ko read karne ke liye ek systematic tareeka provide karta hai.

Is function ka prototype is tarah hota hai: 

```C++
int getopt(int argc, char* const argv[], const char* options);
```

Yahan "options" mein hum arguments ke liye define kiye gaye options flags aur values ko pass karte hain.

Is function ka ek example dekhein:

```C++
#include <iostream>
#include <unistd.h> // library for using getopt function

using namespace std;

int main(int argc, char* argv[]) {
    // option flags ko register karte hain
    const char* options = "m:n:p:"; // options flags: m, n, p
    int c;
    // getopt function ka use karke command line arguments ko read karein
    while ((c = getopt(argc, argv, options)) != -1) {
        switch (c) {
        case 'm':
            cout << "Option -m found with value: " << optarg << endl;
            break;
        case 'n':
            cout << "Option -n found with value: " << optarg << endl;
            break;
        case 'p':
            cout << "Option -p found with value: " << optarg << endl;
            break;
        case '?':
            // agar koi invalid option flag mile to
            cout << "Invalid option: " << optopt << endl;
            break;
        default:
            cerr << "Error while parsing arguments!" << endl;
            exit(1);
        }
    }
    // option flags ke saath arguments ko use karne ke liye
    for (int i = optind; i < argc; i++) {
        cout << "Non-option argument: " << argv[i] << endl;
    }
    return 0;
}
```

Is program mein humne option flags ke saath arguments ko read kiya hai. Terminal mein humne "program_name -m Hello -n World -p 123 invalid" diya hai. Output is tarah hoga:

```
Option -m found with value: Hello
Option -n found with value: World
Option -p found with value: 123
Invalid option: i
Non-option argument: invalid
```

## Dekhein Bhi

- [C++ Command Line Arguments Tutorial](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [Using Getopt to Parse Command-Line Arguments in C++](https://www.gnu.org/software/libc/manual/html_node/Parsing