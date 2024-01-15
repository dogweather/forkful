---
title:                "अस्थायी फाइल बनाना"
html_title:           "Bash: अस्थायी फाइल बनाना"
simple_title:         "अस्थायी फाइल बनाना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Kyu

Temporary files ka upyog karne ka mukhya karan hai ki we apki coding process ko aasan aur organized banate hai. Ye temporary files apko temporary data storage, debugging aur temporary output generation jaise kaamo mein madad karte hai.


## Kaise Kare

Temporary files banane ke liye, aapko `mktemp` command ka upyog karna hoga. Is command ke through aap temporary files ke liye unique names generate kar sakte hai. Niche diye gaye example mein hum dekhenge ki kaise hum ek temporary file bana sakte hai aur usme kuch data store kar sakte hai.

```Bash
#!/bin/bash

# Temporary file create kare
TEMP_FILE=$(mktemp)

# Temporary file mein data store kare
echo "Hello world!" > $TEMP_FILE

# Temporary file ka content print kare
cat $TEMP_FILE
```

Is code mein humne `mktemp` command ka upyog karke ek temporary file banaya aur usme `"Hello world!"` ka data store kiya. Fir humne `cat` command ka upyog karke us temporary file ke content ko print kiya.

### Output:

```Bash
Hello world!
```

Is tarah se aap temporary files create karke apne coding process ko organized aur efficient bana sakte hai.

## Deep Dive

Temporary files banane ke liye, Linux/Unix operating systems mein `tmp` directory ka upyog kiya jata hai. Default tarah se yeh directory `/tmp` mein locate hota hai, lekin is location mein temporary files ko store karne ke liye koi guarantee nahi hoti hai. Isliye, behtar hoga agar aap `mktemp` command mein `-t` flag ka upyog karke apne desired location mein temporary files create kare. Jaise ki:

```Bash
mktemp -t /home/user/Desktop/tempfile
```

Is command ke through, aap apne `/home/user/Desktop` directory mein `tempfile` naam ka temporary file bana sakte hai.

Temporary files banane ke liye, aap `tempfile` ke alawa `mktemp` ke aur bhi options ka upyog kar sakte hai. In options mein se kuch important options ko niche diya gaya hai:

- `-u` - Temporary file ka naam generate kare lekin actual file create na kare.
- `-p` - Location specify kare jaha temporary file create karna hai.
- `-d` - Temporary directory create kare instead of a file.
- `--suffix` ya `-o` - Temporary file ke end mein ek suffix add kare.

In options ka upyog karke aap apne requirements ke according temporary files create kar sakte hai.

## Dekhiye bhi

- [Bash manpage](https://linux.die.net/man/1/bash)
- [mktemp command documentation](https://linux.die.net/man/1/mktemp)