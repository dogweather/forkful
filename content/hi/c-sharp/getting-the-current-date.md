---
title:    "C#: वर्तमान तारीख लेना"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Kyun
Din ko haasil karne ka prayas karne se pahle, aapko current date ka kya upayog ho sakta hai yeh samajhna zaroori hai.

## Kaise Karein
```
C# DateTime currentDate = DateTime.Today;
Console.WriteLine("Aaj ki tareekh: " + currentDate);
```
### Output:
```
Aaj ki tareekh: 8/24/2020
```
Is code snippet mein humne C# programming language ka upayog karke DateTime class ka ek object banaya hai. Isme humne Today property ka upyog kiya hai jo ki current date ko represent karta hai. Uske baad humne WriteLine method ka upayog karke current date ko print kiya hai. Is tarah se hum bahut hi aasaani se current date ko haasil kar sakte hain.

## Gahraai Mein Jaaen
Current date haasil karne ka kaam bahut hi aasan aur zaroori hai. Hum logon ki daily life mein iska upayog humne kai baar dekha hai. Lekin is simple task ki peeche kya logic hai aur iski working kaise hoti hai, yeh jaanne mein bohot saari gahraai hai. DateTime class mein humein kayi aur useful properties bhi mil jaati hain jaise ki AddDays, AddMonths, AddYears, etc. Jinse hum date ko manipulate aur compare kar sakte hain.

## Dekhein Bhi
"See Also"  

[Microsoft Docs - DateTime Class (C# Programming Guide)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=netcore-3.1)  
[C# DateTime Tutorial - W3Schools](https://www.w3schools.com/cs/cs_dates.asp)  
[C# DateTime.Parse Method - GeeksforGeeks](https://www.geeksforgeeks.org/c-sharp-datetime-parse-method/)