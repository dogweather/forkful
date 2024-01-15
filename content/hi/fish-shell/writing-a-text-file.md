---
title:                "टेक्स्ट फाइल लिखना"
html_title:           "Fish Shell: टेक्स्ट फाइल लिखना"
simple_title:         "टेक्स्ट फाइल लिखना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Kyun
Kya tumne kabhi fish shell mein ek text file likha hai? Agar nahi, toh yeh article tumhare liye hai! Text file likhna fish shell mein bahut asaan hai aur yeh tumhe apne programming skills mein izafa karne mein madad karega. Toh chaliye shuru karte hai!

## Kaise Kare
Sabse pehle, fish shell ko apne system mein install kare. Phir, ek text file create kare ya kisi existing file mein changes kare. Text file do tariko se likha ja sakta hai - `echo` command aur `printf` command.

Example:
```Fish Shell
echo "Meri pehli text file" > file.txt
```
Iss command mein humne `echo` command ka use kiya hai jo "Meri pehli text file" ka output `>` operator ke through `file.txt` naam ke file mein likhega.

```Fish Shell
printf "Mera doosra text file" >> file.txt
```
Iss command mein humne `printf` command ka use kiya hai jo "Mera doosra text file" ka output `>>` operator ke through `file.txt` file mein append karega.

## Hiwa
Text file likhne ke alawa, fish shell mein hum apne text files ko edit bhi kar sakte hai. Iske liye `nano` ya `vim` jaise text editors ka use kiya ja sakta hai. Yeh humare programming projects ke liye bahut zaroori ho sakte hai.

## Jyada Gehri Jankari
Fish shell mein text file likhne ke liye aur commands aur operators ka use karne ke liye, hum `man` command ka use kar sakte hai. Yeh hume fish shell ke commands aur unke options ke bare mein jankari deta hai.

## Dekho Bhi
Agar tum fish shell ko sikhna chahte ho toh yeh links tumhare liye faydemand ho sakte hai:
- [Official fish shell website](https://fishshell.com/)
- [Fish shell tutorials](https://fishshell.com/docs/current/tutorial.html)
- [Learn X in Y minutes: Fish Shell](https://learnxinyminutes.com/docs/fish/)