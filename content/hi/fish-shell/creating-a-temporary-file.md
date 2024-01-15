---
title:                "अस्थायी फ़ाइल बनाना"
html_title:           "Fish Shell: अस्थायी फ़ाइल बनाना"
simple_title:         "अस्थायी फ़ाइल बनाना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Kyun

Temporary files ko banane ka mukhya uddeshya hai ki hum ek temporary storage space prapt kar sakein apne program ke liye. Ye temporary files humare kam ke liye temporary storage provide karte hain jo ki humare program ke execution ke dauran require ho sakte hain.

## Kaise Karein

```Fish Shell``` ko temporary file create karne ke liye ```touch``` command ka upyog karein. Command ke sath file ka naam specify karein, jaise ki ```touch temp.txt``` aur ye ek temporary file create kar degi. Agar hum ```ls``` command se dekhein to ye file ```temp.txt``` naam se list hogi.

## Deep Dive

```touch``` command ke alawa bhi temporary file create karne ke aur options available hain. Ismein hum ```mktemp``` command ka upyog kar sakte hain jo ki random naam se file create kar degi. Example ke liye, ```mktemp``` command ka upyog karke ```mktemp -p ~/Desktop``` karke hum ek temporary file create kar sakte hain jise humare desktop par save hoga.

Temporary files create karne ke liye ek aur prakriya hai, jise "here document" kehte hain. Ismein hum ek temporary file ka content dynamically generate kar sakte hain. Example ke liye, neeche diye gaye code block mein humein ek temporary file create karna hai jiska naam "temp.txt" hoga aur usmein kuch text add karna hai.

```Fish Shell
cat > temp.txt << EOF
This is a temporary file.
EOF
```

Agar hum ```cat temp.txt``` command se dekhein to humein output mein "This is a temporary file." dikhega.

## Dekhte Hain

Hum ```touch```, ```mktemp``` aur "here document" ka upyog karke temporary files create kar sakte hain. Inke alawa bhi aur bhi options hote hain jinhe aap explore kar sakte hain. Maine niche kuch links diye hain jinhe aap padhkar aur samajhkar apne projects mein temporary files ka upyog kar sakte hain.

## Dekhiye Bhi

- [Linuxize - How to Create Temporary Files in Shell Scripting](https://linuxize.com/post/create-temporary-files-in-shell-scripting/)
- [Geomajas - How to Create Temporary Files in the Shell](https://www.geomajas.org/tutorials/file-handling/how-to-create-temporary-files-in-the-shell/)
- [ShellHacks - How to Create Temporary Files in Shell Scripts](https://www.shellhacks.com/create-temporary-file-shell-script-linux/)