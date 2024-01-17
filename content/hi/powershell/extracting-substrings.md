---
title:                "उप-स्ट्रिंग हटाना"
html_title:           "PowerShell: उप-स्ट्रिंग हटाना"
simple_title:         "उप-स्ट्रिंग हटाना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Kya & Kyu?
Substring ko extract karna, kisi bhi shabd, var ya samast shreni mein se chote tukdon ko alag karke unka upyog karna hai. Programmers iska upyog karke lambi shabdo wali database entries ko dhoondhne mein madad lete hain ya phir specific patterns ko khojne mein. Substring extraction techniques data processing mein bhi bahut kaam aate hain.

## Kaise Karein?
Substring extraction ka simplest tareeka hai "Substring" command ka upyog. Iske liye hum "PowerShell" mein ```$string.Substring()``` command ka upyog karenge. Yeh command deta hai ek tukda shabd, var ya samast shreni ke bheetar se, jo hum specify karte hain.

Jaise ki:

``` PowerShell
$string = "programming"
$string.Substring(4,5)
```

Iska output hoga:

``` PowerShell
rammi
```

Iss tareeke se hum bina kuch extra effort ke substring extract kar sakte hain.

## Gehri Jhaank:
Substring extraction bahut hi common technique hai data processing mein. Iska upyog bahut ideas mein kiya jata hai, jaise queries ko filter karne ya specific data points ko dhoondne ke liye. Iske alawa, kuch aur tareeke bhi hain substring extraction ke, jaise ki ```Split``` and ```Select-String``` commands.

Split command ek jagah se dusri jagah tak ka shabdon ko alag karta hai, jabki Select-String command specific patterns ya strings ko khojne mein madad karta hai.

## Dekhein Bhi:
1. [Microsoft Docs: Substring Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring)
2. [Tutorialspoint: Substring in PowerShell](https://www.tutorialspoint.com/powershell-Powershell-Powershell-commands/substring_PowerShell.htm)

Yeh dono resources aapko zyada jankari aur examples pradan karenge substring extraction ke liye.