---
title:                "Javascript: अस्थायी फाइल बनाना"
simple_title:         "अस्थायी फाइल बनाना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

"Kyu: Temporary File create karne ka kya fayda hai?"

Temporary files, jaise ki naam se bhi pata chalta hai, kuch samay ke liye hi bante hai aur unka upyog baad mein khatam ho jata hai. Temporary files create karne ka sabse bada karan hai memory management. Jab hum koi bada kaam karte hai, jaise ki photo editing ya video editing, toh hume temporary files ka use karke apni memory ko conserve karna padhta hai. Isse humare computer ki performance aur speed mein bhi behad farak padhta hai.

"Kaise Kare: Temporary File ka upyog kaise kare?"

Temporary files ko create aur access karne ke liye hum Javascript language ka use kar sakte hai. Niche diye gaye code block mein diye gaye code se aap asani se temporary file ko create aur access kar sakte hai. 

```Javascript
//Temporary file create karna
var fs = require('fs'); // 'fs' module ko import karna
var tempFile = fs.createWriteStream('temporaryfile.txt'); //'temporaryfile.txt' naam ka file create karna
tempFile.write('Yeh temporary file hai.'); //file mein text likhna
//Temporary file access karna
var tempFile = fs.readFileSync('temporaryfile.txt').toString(); //'temporaryfile.txt' file ko padhkar string mein convert karna
console.log(tempFile); //file mein likhe gaye text ko console mein print karna
```

Output: ```Yeh temporary file hai.```

"Khwabon ki Khaatir: Temporary Files par gehri jankari"

Temporary files ko create karne ke liye do tarah ke functions hote hai: ```createWriteStream()``` aur ```createReadStream()```. ```createWriteStream()``` function hume temporary file ko write karne ki permission deta hai jabki ```createReadStream()``` function hume temporary file ko read karne ki permission deta hai. Temporary files ko create karne ke baad hume unhe delete kar dena zaruri hai. Iske liye hum ```fs.unlinkSync()``` function ka use kar sakte hai. 

Temporary files create karne mein hume ```fs.writeFile()``` function bhi use kar sakte hai. Isme hum parameter ke taur par ek callback function pass karte hai jo temporary file create karne ke baad execute hota hai. 

Jaise ki temporary files ka istemal memory management ke liye hota hai, isse humara computer ya device faster aur efficient ho jata hai. Lekin agar hum temporary files ko sahi se manage na karein toh yeh hamare computer ke liye samasyao ka karan bhi ban sakte hai.

Ab aap temporary files ke bare mein gehri jankari rakhte hai aur unhe create aur access kar sakte hai.

"See Also:"

- [Javascript File System Module Documentation](https://nodejs.org/api/fs.html)
- [How to Manage Temporary Files in Nodejs](https://medium.com/@kevinhsueh/how-to-manage-temporary-files-in-nodejs-9125b551ed68)
- [Why Temporary Files are Important for Memory Management](https://www.techpakistan.org/2018/05/05/temporary-files-importance-for-memory-management/)