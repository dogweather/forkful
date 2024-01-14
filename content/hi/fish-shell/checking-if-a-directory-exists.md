---
title:                "Fish Shell: डायरेक्टरी मौजूद है या नहीं पर जाँच करना"
simple_title:         "डायरेक्टरी मौजूद है या नहीं पर जाँच करना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

"## Kyon: Pratiksha Karte Huye Directory Ko Kaise Check Karein"

Kabhi kabhi humein apne program mein kisi directory ki upasthiti ko check karna padta hai. Yah jaruri ho sakta hai kisi specific file ko dhoondhne ke liye, ya fir ek directory mein kuch files ko read ya modify karne ke liye. Isliye, directory ki presence check karna, ek zaroori part hota hai humare coding process ka. Is blog post mein hum aapko batayenge ki kisi directory ki presence kaise check kar sakte hain, Fish Shell ke saath.

"## Kaise Kare"

Fish Shell mein, hum `test` command ka use karke directory ki presence check kar sakte hain. Yeh command ek conditional check ke liye use hoti hai. Agar humein kisi specific directory ki presence check karni hai, toh hum `test -d` command ka use karenge, jahan `-d` `test` command ka ek flag hai, jo directory ki presence check karta hai.

### Raasta #1: Directory ki Presence Check karna
```Fish Shell
test -d /path/to/directory
```
Yeh command `true` ya `false` output degi, jahan `true` hoga agar directory upasthit hai aur `false` hoga agar directory na ho.

### Raasta #2: Agar Directory Upasthit Hai Toh Use Ek Conditional Block Mein Add Karein
```Fish Shell
if test -d /path/to/directory
    echo "Directory upasthit hai!"
end
```
Yahan hum `if` aur `end` keywords ka use kar rahe hain, jismein `if` conditional check ko define karta hai, aur `end` uske baad ka code block ko define karta hai.

### Raasta #3: Output Ko Variable Mein Store Karein
```Fish Shell
set directory_present (test -d /path/to/directory; and echo "true")
```
Yahan humne `test -d` command ke output ko variable `directory_present` mein store kiya hai. Is raaste mein, hum `; and` use kar rahe hain, jo `test` command ke output ko `echo` command ke saath combine karta hai.

"## Deep Dive"

`test` command, Fish Shell ke ek built-in command hai, jo file aur directory ko check karne ke liye use hota hai. Yeh command 3 levels of prescription dekhta hai - user, group, aur world. Is command ka format hai `test [switch] file`. Humne is post mein, `-d` switch ka use kiya hai, jiske zariye hum specific directory ko check kar rahe hain.

"## See Also"

- [Fish Shell documentation on `test` command](https://fishshell.com/docs/current/cmds/test.html)
- [Tutorialspoint article on `test` command in Fish Shell](https://www.tutorialspoint.com/unix_commands/test.htm)
- [Maanava Adhikar Foundation blog on basic Fish Shell commands](https://www.manavadhikarfoundation.org/2018/05/27/basic-fish-shell-commands/)