---
title:                "स्ट्रिंग की लंबाई ज्ञात करना"
aliases:
- /hi/bash/finding-the-length-of-a-string/
date:                  2024-01-20T17:47:06.996832-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग की लंबाई ज्ञात करना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
String ki length jaanna matlab ye hai ki us string me kitne characters hain. Programmers isliye string ki length jaan'na chahte hain taki wo data ko validate kar sakein, loops chala sakein, ya fir substring kaam karne ke liye.

## How to: (कैसे करें:)
Bash me string ki length pata karna simple hai. Neeche di gai code me dekho:

```Bash
# String ko variable me save karo
my_string="Hello, नमस्ते!"

# {#variable} use karte hue length pata karo
length=${#my_string}
echo "String length is: $length"
```

**Output:**
```
String length is: 16
```

## Deep Dive (गहन जानकारी)
Bash me string ki length pata karne ka tarika kafi seedha hai, jo ki shell scripting ke shuruaati version se hi uplabdh hai. `#` symbol ka prayog variable ke naam ke saath kiya jaata hai, jo ki string ke characters ki sankhya ko lautaata hai.

Kuch aur tariko se bhi hum length pata kar sakte hain, jaise ki `expr` command ka use karke:

```Bash
length_with_expr=$(expr length "$my_string")
echo "String length with expr is: $length_with_expr"
```

Ya fir `awk` ka use karke:

```Bash
length_with_awk=$(echo "$my_string" | awk '{print length}')
echo "String length with awk is: $length_with_awk"
```

Lekin aam taur par, `${#variable}` syntax sabse saaf aur tez hota hai.

## See Also (और जानिए)
- [Bash String Manipulation Guide](https://www.gnu.org/software/bash/manual/)
- [Advanced Bash-Scripting Guide](http://www.tldp.org/LDP/abs/html/index.html)
- [`expr` command in detail](https://man7.org/linux/man-pages/man1/expr.1.html)
- [`awk` programming language](https://www.gnu.org/software/gawk/manual/gawk.html)
