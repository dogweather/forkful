---
title:                "``प्रोग्रामिंग में नमूने  पर लिखना``"
html_title:           "C++: ``प्रोग्रामिंग में नमूने  पर लिखना``"
simple_title:         "``प्रोग्रामिंग में नमूने  पर लिखना``"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Kyun

Aksar hum programming mein kaam karte huye errors aur bugs se rubaru hote hain. Lekin kya aapko pata hai ki hum kisi bhi error ya warning ko catch kar sakte hain aur usse behtar code likh sakte hain? Isliye, standard error mein likhna humare liye kaafi faydemand ho sakta hai.

## Kaise

Humein apne code mein "cerr" function ka use karna hota hai taki hum kisi bhi error ya warning ko standard error output stream mein likh sakein. Neeche diye gaye C++ code block mein ek example diya gaya hai:

```C++
#include <iostream>

using namespace std;

int main()
{
  int num1 = 10, num2 = 0;
  float result;

  // Divide by zero error
  if(num2 == 0)
  {
    cerr << "Error: Cannot divide by zero!" << endl;
    return 1;
  }

  // Calculate result and output
  result = num1 / num2;
  cout << "Result: " << result << endl;

  return 0;
}
```

Output:

Error: Cannot divide by zero!

Is code mein humne "cerr" function ka use kiya hai takki error message standard error output stream mein aaye aur humein pata chal sake ki code kahan par error de raha hai. Is tarah se hum apne code ko troubleshoot kar sakte hain aur behtar code likh sakte hain.

## Gehri Jhaank

Standard error output stream, jiske liye "cerr" function ka use kiya jata hai, humare liye kaafi mahatvapurna hai. Iske through hum apne errors ko handle kar sakte hain aur apne code ko debug kar sakte hain. Iske alawa, hum standard error output stream ko redirect bhi kar sakte hain aur apne errors ko log file mein save kar sakte hain.

Isliye, jab bhi hum code likhein, humein standard error output stream ka use karna chahiye taki humare code mein koi bhi error ho, hum use asani se troubleshoot kar sakein.

## Dekhein Bhi

- [C++ Language Tutorial - Standard Error](https://www.cplusplus.com/doc/tutorial/files/)
- [Tutorialspoint - C++ Input/Output Streams](https://www.tutorialspoint.com/cplusplus/cpp_input_output.htm)
- [Guru99 - C++ Error Handling](https://www.guru99.com/c-plus-plus-exception-handling.html)