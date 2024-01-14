---
title:    "C#: भविष्य या अतीत में तारीख की गणना"
keywords: ["C#"]
---

{{< edit_this_page >}}

"## Kyun"

Aaj kal hamare paas bahut saare technology aur gadgets hai jo humare zindagi ko aasan banate hai. Inme se ek hai programming languages, jisme se C# ek popular language hai jo bahut saari fields mein use kiya jaata hai. Aaj hum baat karenge date ko calculate karne ki, future ya past mein, aur usse kyun engage hona chahiye.

"## Kaise Kare"

Yadi aap date ke saath kaam karte hai, jaise ki kisi project ya task ki deadline set karna, toh aapko date ko aage ya peeche calculate karne ki zarurat pad sakti hai. C# mein iske liye aapko DateTime class ka istemaal karna hota hai.

Ek simple example ke liye, hum ek date ko 7 din aage calculate karenge. Iske liye hum pehle DateTime object banayenge aur usse current date se initialise karenge. Fir hum usme 7 din add kar denge.

```C#
DateTime currentDate = DateTime.Today;
DateTime futureDate = currentDate.AddDays(7);
```

Is code se humne current date liya aur usme 7 din add karke future date calculate ki. Is tarah se aap future ya past mein bhi date ko manipulate kar sakte hai. Iske alawa hum time zones aur leap years ko bhi consider kar sakte hai.

"## Gahre Khayal"

DateTime class ke saath kaam karte hue, aapko kuch important points ko dhyaan rakhna hoga. Jaise ki aapko time zones ko handle karna hoga, kyunki date aur time ki values alag-alag time zones mein different ho sakti hai. Iske alawa DateTime object immutable hota hai, iska matlab hai ki aap usse modify nahi kar sakte hai, lekin aap naye objects bana sakte hai jinse aap date ko manipulate kar sakte hai. Aur leap years ko handle karne ke liye bhi aapko careful hona hoga.

"## Dekhiye Bhi"

Yadi aapko date ko calculate karne mein aur bhi details chahiye, toh aap in links ko refer kar sakte hai:

- Microsoft Docs: https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0
- Tutorialspoint: https://www.tutorialspoint.com/csharp/csharp_date_time.htm
- C# Corner: https://www.c-sharpcorner.com/blogs/manipulation-of-datetime-typed-value-in-net2