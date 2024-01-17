---
title:                "रैंडम नंबर्स उत्पन्न करना"
html_title:           "PowerShell: रैंडम नंबर्स उत्पन्न करना"
simple_title:         "रैंडम नंबर्स उत्पन्न करना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Kya aur Kyu?

Random numbers generate karne ka matlab hai ki hum kisi bhi tarah ke numbers ko randomly generate kar rahe hain, jisse ki hum un numbers ko predict na kar sakein. Programmers iska use tabhi karte hain jab unhe unpredictable data ki zaroorat hoti hai jaise password generate karna ya game development mein.

## Kaise Karein?

```PowerShell
# Sampoorn sankhya generate karna
Get-Random

# Kuchh niymit sankhya generate karna
Get-Random -Minimum 1 -Maximum 10

#  Shaamil hote hue sankhya ko rahe
Get-Random -InputObject 1, 2, 3, 4, 5

# Random password generate karna
GeneratePassword -Length 10 -Symbols

# Random color generate karna
Get-Random -InputObject "Red", "Blue", "Green", "Yellow"
```
Output:
```
6
7
3
h5%9K3$x@&
Blue
```

## Gehri Jankari

Random numbers ko generate karne ka tarika bahut purana hai aur iska use cryptography mein bhi hota hai. Agar kisi ko random numbers generate karne ke liye Microsoft .NET Framework ki zaroorat hai toh ve powershell ko use kar sakte hain. Aur agar aapko specific type ke random numbers chahiye toh aap ```System.Random``` class ka use kar sakte hain.

## Zaroor Dekhein

https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-5.0
https://www.powershellguru.com/random-numbers-generation-in-powershell/