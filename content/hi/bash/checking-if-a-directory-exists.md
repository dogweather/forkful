---
title:                "डायरेक्ट्री का अस्तित्व जांच करना"
html_title:           "Bash: डायरेक्ट्री का अस्तित्व जांच करना"
simple_title:         "डायरेक्ट्री का अस्तित्व जांच करना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why
Aksar humare paas kuch aise bash scripts hote hai jinhe hum directories ke saath kaam karne ke liye banate hai. Lekin kabhi kabhi hume yeh confirm karna hota hai ki kya woh directory asal mein maujood hai ya nahi. Isiliye hume directory ke maujoodgi ko check karne ki zarurat padti hai.

## How To
Agar aap bhi directory ke maujoodgi ko check karna chahte hai, toh aapko iss tarah se bash code likhna hoga:
```Bash
if [ -d /path/to/directory/ ]; then
    echo "Directory exists"
else
    echo "Directory does not exist"
fi
```
Iss code mein humne `if` statement andar `[-d]` flag ka use kiya hai jo directory ke upar maujood operations perform karne ke liye hota hai. Agar directory maujood hota hai, toh hume `Directory exists` ka output milega, nahi toh `Directory does not exist` ka output milega.

## Deep Dive
Bash mein directory ke maujoodgi ko check karne ke liye hum `[-d]` flag ke alawa aur bhi kuch flags ka use kar sakte hai. Jaise `-f` flag directory ke upar file operations perform karne ke liye hota hai. Iske alawa, aap `[-e]` flag ka use karke file ya directory ke upar operations perform kar sakte hai.

Agar aapko directory ke permission bhi check karni hai, toh aap `[-r]` flag ka use karke directory ke read permission ko check kar sakte hai. Iske alawa, aap `[-w]` flag ka use karke write permission ko bhi check kar sakte hai.

## See Also
Iss article mein humne bash mein directory ke maujoodgi ko check karne ka tareeka sikhaya hai. Agar aapko bash scripting aur directories ke baare mein aur jaanna hai, toh yeh links aapke kaam aa sakte hai:
- [Bash Scripting Tutorial](https://www.howtogeek.com/102723/how-to-write-a-simple-bash-script/)
- [Working with Directories in Bash](https://linuxhint.com/working_directories_bash/)
- [Directory Operations in Bash](https://ubuntu.com/tutorials/command-line-for-beginners#6-directory-operations)