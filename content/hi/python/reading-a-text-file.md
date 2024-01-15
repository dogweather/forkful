---
title:                "एक टेक्स्ट फ़ाइल पढ़ना"
html_title:           "Python: एक टेक्स्ट फ़ाइल पढ़ना"
simple_title:         "एक टेक्स्ट फ़ाइल पढ़ना"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Kyun
Kai baar humein text files padhne ki zarurat hoti hai, jaise ki kisi program mein jaankariyan ko padhne ke liye, ya fir kisi data ko analyze karne ke liye. Agar aapko Python mein kaam karna hai, toh text files ko padhna ek important skill hai.

## Kaise Padhein Text File Python mein
Text files ko Python mein padhna kaafi easy hai. Sabse pehle, aapko `open()` function ka use karna hoga. Iss function mein aapko do arguments pass karne hote hain - file ka naam aur uss file ko kis mode mein open karna hai. Jaise: `file = open('sample.txt', 'r')` yahaan `sample.txt` file ka naam hai aur `r` mode hai, jo ki read mode hai.

Ab hum `read()` method ka use karenge `file` object pe. Iss method se hum file ka pura content padh sakte hain. Jaise: `file_content = file.read()` yahaan `file_content` variable mein humari file ka pura content store ho jaayega.

Ab, agar hume kuch specific lines ya words padhne hain, toh hum `readlines()` method ka use kar sakte hain. Iss method se hum file ka har ek line ko list ke form mein padh sakte hain. Jaise: `file_content = file.readlines()` yahaan `file_content` variable mein list of lines store ho jaayega.

Finally, humein `close()` method ka use karna hai `file` object ko close karne ke liye. Jaise: `file.close()`.

Iss article mein humne ek basic example dekha, lekin aapko agar aur details chahiye toh aap humari See Also section mein diye gaye links ko check kar sakte hain.

```Python
file = open('sample.txt', 'r')
file_content = file.read()
print(file_content)
file.close()
```

## Deep Dive
Python mein text file read karne ke liye kaafi saare methods available hote hain jaise ki `read()`, `readlines()`, `readline()`, etc. Iske saath hi, file ko open karne ke liye bhi kaafi modes hote hain jaise ki `r`, `r+`, `w`, `a`, etc. Aur in modes ka use aapki file pe depend karta hai.

Iss article mein humne sirf text files ke read karne ke baare mein baat ki hai, lekin aap text files ko write aur manipulate bhi kar sakte hain Python mein. Aapko agar in topics pe aur jaankari chahiye toh aap humari See Also section mein diye gaye links ko check kar sakte hain.

## Dekhein Bhi
Yahaan kuch aur articles aur resources hain jo aapke liye helpful ho sakte hain:

- [Reading and Writing Files in Python](https://realpython.com/read-write-files-python/)
- [Python File Handling](https://www.w3schools.com/python/python_file_handling.asp)
- [Python Tutorial: File Objects and Methods](https://www.datacamp.com/community/tutorials/file-objects-python)
- [Manipulating Files with Python](https://realpython.com/working-with-files-in-python/)
- [Python Text File Processing - Reading and Writing Files](https://www.geeksforgeeks.org/python-text-processing-reading-and-writing-text-files/)

Dhanyawaad aur happy coding!