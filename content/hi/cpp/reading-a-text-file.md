---
title:                "टेक्स्ट फाइल पढ़ना"
html_title:           "C++: टेक्स्ट फाइल पढ़ना"
simple_title:         "टेक्स्ट फाइल पढ़ना"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Kyun

Kai baar hume kisi program ko chalane ke liye ek text file ki jarurat padti hai. Text file me data organized tarah se hota hai aur isse hum sahi tarike se program ke andar load kar sakte hai. Aise cases me, hum text file ko padhna seekhna chahte hai taaki hum apne programming skills ko improve kar paye aur apne programs me data ko sahi tarah se handle kar paye.

## Kaise Kare

Text file ko padhne ke liye, hume `fstream` library ka use karna hoga. Ye library hume file ko open karne aur usme se data ko read karne ka code provide karti hai. Iske liye, hume `ifstream` object banakar usko file ka naam aur file mode dekar initialize karna hoga. Iske baad, hum `>>` operator se file se data ko read kar sakte hai.

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main() {
  ifstream file; // ifstream object banaye
  file.open("example.txt", ios::in); // file ko open kare

  if (!file.is_open()) { // agar file open nahi hoti, error message print kare
    cout << "File could not be opened." << endl;
    return 1;
  }

  string data;

  while (file >> data) { // jab tak file se data read ho raha hai, use print kare
    cout << data << " ";
  }

  file.close(); // file ko close kare

  return 0;
}
```

`example.txt` file ke content agar "Hello World" hai, tab is program ka output hoga:
```
Hello World
```

## Deep Dive

`fstream` library ka use karke hum multiple file modes me text file ko open kar sakte hai. Kuch modes me hum sirf file me se data read kar sakte hai, jabki kuch modes me hum file me naye data ko write bhi kar sakte hai. Isse hum apne programs me file handling ko aur bhi versatile bana sakte hai.

In addition, hum text file me se different data types ko bhi read kar sakte hai using `>>` operator. Jaise ki agar hume int, float, double, ya char ko file se read karna hai, to hum us datatype ke according variable ko define karke usko `>>` operator se read kar sakte hai. Is tarah ki details ke liye, aap `fstream` library ki documentation dekh sakte hai.

## Dekhiye Bhi

- [https://www.geeksforgeeks.org/working-text-files-cpp/](https://www.geeksforgeeks.org/working-text-files-cpp/)
- [https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)