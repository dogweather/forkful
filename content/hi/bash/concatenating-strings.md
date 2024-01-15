---
title:                "स्ट्रिंग सम्मिलित करना"
html_title:           "Bash: स्ट्रिंग सम्मिलित करना"
simple_title:         "स्ट्रिंग सम्मिलित करना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why
Kya aapko kabhi apne Bash scripts mein alag-alag strings ko ek saath jodna pada hai? Ya phir aapko ek variable mein do alag strings ko combine karna hai? Agar haan, to aap string concatenation ke bare mein zarur jante honge. String concatenation ek aasan aur faydemand tarika hai jo kisi bhi programming language mein strings ko combine karne ke liye istemal kiya jaata hai. Is article mein hum aapko Bash mein string concatenation ke baare mein puri jaankari denge.

## How To

```Bash
# Syntax for string concatenation using variables
new_string=$string1$string2

# Concatenating strings with literal text
full_string="Hello, my name is $name. I am $age years old."

# Using command substitution to concatenate output of commands with strings
dir=$(ls)
result="The contents of the directory are: $dir"
```
Example Output:

```Bash
# For variable concatenation
string1="Welcome "
string2="to my world!"
echo $string1$string2
# Output: Welcome to my world!

# For concatenating strings with literal text
name="John"
age=25
echo "Hello, my name is $name. I am $age years old."
# Output: Hello, my name is John. I am 25 years old.

# For command substitution
dir=$(ls)
echo "The contents of the directory are: $dir"
# Output: The contents of the directory are: file1.txt file2.txt file3.txt
```

## Deep Dive
Bash mein string concatenation ke liye do main tariko ka istemal kiya jaata hai: variable concatenation aur literal text concatenation. Variable concatenation mein hum variables ko ek saath likhkar join karte hain. Literal text concatenation mein hum literal text aur variables ko double quotes ("") ke andar likhte hain. Double quotes mein likhe hue variables kisi bhi command ke output se replace ho sakte hain.

Humein bash mein command substitution ka bhi istemal karke strings ko concatenation kiya ja sakta hai. Command substitution mein hum kisi bhi command ko $(...) ke andar likhte hain, jiske output ko hum variables ya literal text ke saath combine kar sakte hain.

String concatenation ek powerful tool hai jo programming mein strings ko manipulate karne ke liye bahut faydemand hai. Isse hum apne scripts ko aur flexible aur readable bana sakte hain.

## See Also

- [Bash Guide for Beginners](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [Bash String Manipulation](https://linuxize.com/post/bash-string-manipulation/)
- [Bash Command Substitution](https://www.baeldung.com/linux/bash-command-substitution)