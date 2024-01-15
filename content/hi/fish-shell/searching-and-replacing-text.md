---
title:                "टेक्स्ट को खोजना और प्रतिस्थापित करना"
html_title:           "Fish Shell: टेक्स्ट को खोजना और प्रतिस्थापित करना"
simple_title:         "टेक्स्ट को खोजना और प्रतिस्थापित करना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Kyun
Tum logon ko pata hai, computer par kaam karte waqt hum kabhi-kabhi text ko badalna ya replace karna chahte hain. Jaise ki agar hume ek poora document me se ek particular word ko delete karna hai ya phir kisi word ki spelling theek karni hai. Yeh sab kaam manually karne me humara bahut time waste ho jaata hai. Lekin, fish shell ka use karke hum is process ko asaan aur tezi se kar sakte hain.

## Kaise Karein
Fish shell me text search and replace karne ke liye hum `sed` command ka use karenge. Iske liye sabse pehle hume `sed` command ko install karna hoga. Iske liye hum terminal me yeh command likhenge:
```Fish Shell
sudo apt-get install sed
```
Ab, hume `sed` command ka syntax jaanna hoga. Yeh command 3 parts se bana hai - `address`, `command` aur `input file`. Iske alawa, hum replace karne ke liye ek aur argument yaani `replacement` bhi use kar sakte hain.

Ab, maan lijiye hume ek file me se ek particular word ko delete karna hai. Toh hum `sed` command ko iss tarah use karenge:
```Fish Shell
sed /word/d input_file.txt
```
Yahaan `/word/` ka matlab hai hume jo bhi word delete karna hai, woh `input_file.txt` me se humare liye search karega. Iske baad, humara naya output terminal par show hoga.

Agar hum kisi word ki spelling ko replace karna chahte hain, toh hum iss command ko use kar sakte hain:
```Fish Shell
sed s/old_word/new_word/g input_file.txt
```
Yahaan `/old_word/` ka matlab hai humare document me se hume woh word delete karna hai aur `/new_word/` ka matlab hai woh word jo hum iss file me daalna chahte hain.

## Deep Dive
`sed` command bahut powerful hai aur hum iss command ki madad se bahut saare text manipulation tasks ko asaan se kar sakte hain. Iske alawa, hum iss command me `regex` bhi use kar sakte hain jiske baare me ek alag se article likha ja sakta hai.

Iske alawa, hum iss command ko use karke file ko modify bhi kar sakte hain. Hum iss command me `-i` flag ka use karke humare original file ko overwrite kar sakte hain. Isse hume ek naya file create karne ki zaroorat nahi padegi.

## Dekhein Bhi
1. [Fish Shell GitHub page](https://github.com/fish-shell/fish-shell)
2. [Fish Shell tutorial by The Primeagen](https://www.youtube.com/watch?v=-M2amRgTu-8)
3. [Regex tutorial by freeCodeCamp.org](https://www.youtube.com/watch?v=rhzKDrUiJVk)

# Aur Jaanein
Lagbhag har commands me, hum terminal se bahut saare text manipulation tasks asaanise kar sakte hain. Isliye, aapko apne terminal ke commands ko acche se jaan lena chahiye. Aasha karte hain aapko yeh article helpful laga hoga. Keep coding!