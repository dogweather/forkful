---
title:                "डीबग आउटपुट छापना"
html_title:           "Gleam: डीबग आउटपुट छापना"
simple_title:         "डीबग आउटपुट छापना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

Kya Hai Aur Kyu?
Printing debug output aam taur par programmers dvaara errors aur bugs ko diagnose aur debug karne ke liye kiya jata hai. Ye ek aasaan aur prabhavi tareeka hai jo code ke sahi chalne ke samasyao ko pahchanne me madad karta hai.

Kaise Kare?
Dekhiye, debug output print karne ke liye Gleam me bahut sare tarike hain. Hum kuch aasan aur upyukt tarike ko samjhenge. Sabse pehle, hum ```debug!()``` function ka upyog karke console par kuch khaas meassage print kar sakte hain:
```Gleam
debug!("Hello, World!");
```
yeh ```Hello, World!``` ka output dega. Agar hum input bhi add karna chahate hain, to hum ye tareeka istemaal kar sakte hain:
```Gleam
let name = "John";
debug!("Hello, {}", name);
```
Iska output hume ```Hello, John``` dikhayega.
Ek aur useful tarika hai ```dbg()``` function ka upyog. Ye hume value aur uski sahi data type ke saath output dikhata hai:
```Gleam
dbg!(5 * 2);
```
Iska output hume ```10 [i32]``` dega, yani ki ```5 * 2``` ki value aur uski data type.

Deep Dive
Debug output ka upyog hume code ke errors aur bugs ko diagnose karne me madad karta hai. Iske alawa, ye code ko test karne me bhi uttam hai. Isliye, debug output ka istemaal karna behad zaroori hai. Agar hum Gleam code ke bina debug output ke istemaal ke baare me baat kare, to ye humare liye musibat ho sakti hai kyunki debugging me kafi time aur resources kharch hote hain. Kuch aur programming languages ki tarah, Gleam me bhi logging library suvidhayein vaisi hi hain jo aapko console par output print karne me madad karegi.

See Also
Ab jo aap kisi bhi programming language me log banate hain, unhe understand karna aur uski functionality ko smajhna zaroori hai. Isliye uske liye aapko ek debugger ki zaroorat hoti hai. [Yahan](https://gleam.run/articles/debug-theory/) aap ek detailed article pa sakte hain jisme Gleam me debug output ka theoretical perspective diya gaya hai. Iske alawa, aapko kuch example Gleam projects bhi dekhne chahiye jo console par debug output ka istemaal karte hain. Isse aapko ye samajhne me madad milegi ki hum log kaise apna code debug kar sakte hain.