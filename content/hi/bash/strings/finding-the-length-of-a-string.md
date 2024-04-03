---
date: 2024-01-20 17:47:06.996832-07:00
description: "String ki length jaanna matlab ye hai ki us string me kitne characters\
  \ hain. Programmers isliye string ki length jaan'na chahte hain taki wo data ko\u2026"
lastmod: '2024-03-13T22:44:52.606821-06:00'
model: gpt-4-1106-preview
summary: String ki length jaanna matlab ye hai ki us string me kitne characters hain.
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0940 \u0932\u0902\
  \u092C\u093E\u0908 \u091C\u094D\u091E\u093E\u0924 \u0915\u0930\u0928\u093E"
weight: 7
---

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
