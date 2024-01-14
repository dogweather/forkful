---
title:    "Bash: टेक्स्ट फ़ाइल पढ़ना"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Kyon
Agar aap ek programmer hai aur Bash programming mein apne skill ko improve karna chahte hai toh aapko text file ko padhna aur usse manipulate karna sikhna bahut zaruri hai. Isse aap apne code mein kisi bhi file se data retrieve aur use kar sakte hai. Is article mein hum aapko Bash programming se text file ko kaise padhna hai uske baare mein detail se batayenge.

## Kaise Kare
Text file ko padhna bahut hi simple hai Bash mein. Sabse pehle hum `cat` command ka use karenge, jiski madad se hum text file ko terminal mein display kar sakte hai. Yeh command kisi bhi file ka content dikhane ke liye use ki jaati hai. Iske baad hum `while` loop ka use karenge, jisse hum file ko line by line read kar sakte hai.

```Bash
#!/bin/bash
# File name: file_read.sh

cat sample.txt | while read line; do
    echo $line
done

# Output:
# This is line 1
# This is line 2
# This is line 3
# This is line 4
```

Jaisa ki hum dekh sakte hai, `cat` command hume apne file ke content ko display kar rahi hai aur `while` loop hume ek ek line ko read karne mein help kar raha hai. Lekin agar hum sirf file ke content ko read karna chahte hai aur usse manipulate nahi karna chahte hai, toh hum `cat` command ki jagah `less` command ka use kar sakte hai.

```Bash
less sample.txt

# Output:
# This is line 1
# This is line 2
# This is line 3
# This is line 4

```

Is tarah se hum text file ko Bash mein padh sakte hai.

## Deep Dive
Ab hum text file ko padhne ke liye aur detail mein kaise code likh sakte hai uske baare mein dekhte hai. Agar hume sirf kuch specific lines ko read karna hai aur baaki ko ignore karna hai toh hum `head` aur `tail` commands ka use kar sakte hai.

```Bash
#!/bin/bash
# File name: selective_read.sh

head -n 3 sample.txt # Reads first 3 lines 
echo "----------"
tail -n 2 sample.txt # Reads last 2 lines

# Output:
# This is line 1
# This is line 2
# This is line 3
# ----------
# This is line 3
# This is line 4
```

Is tarah se hum apni requirement ke hisab se lines ko read kar sakte hai. Hum file mein koi specific word ya pattern search karna chahte hai toh hum `grep` command ka use kar sakte hai.

```Bash
#!/bin/bash
# File name: word_search.sh

grep "line 2" sample.txt # Searches for line 2 in file

# Output:
# This is line 2
```

Is tarah se hum file mein se specific information ko retrieve kar sakte hai.

## Aur Bhi Jaankari Ke Liye
Agar aap aur bhi jaankari chahte hai Bash programming ke baare mein toh aap in links ko refer kar sakte hai.

1. [Bash Tutorial by Shell Scripting Tutorial](https://www.shellscript.sh/)
2. [Introduction to Bash Scripting by Linuxize](https://linuxize.com/post/bash-scripting-introduction/)
3. [Bash Scripting Cheat Sheet by Devhints](https://devhints.io/bash)

## Dekhiye Bhi
Is article mein humne Bash programming mein text file ko kaise padhte hai uske baare mein jana. Yeh ek important skill hai jo har programmer ko sikhna chahiye. Agar aapko yeh article pasand aaya toh aap isse share karke dusre programmers ko bhi help kar sakte hai. Happy coding!