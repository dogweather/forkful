---
title:                "Swift: स्ट्रिंग कैपिटलाइज करना"
simple_title:         "स्ट्रिंग कैपिटलाइज करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Kyun:
Kya aapne kabhi socha hai ki Swift mein strings ko capital letters mein convert karna kyon zaruri hai? Ye bahut sari coding languages mein common hai aur kaafi use hota hai. Iska kaaran hai ki capital letters padhne aur samajhne mein aasaan hote hain, aur kuch cases mein ye data ko sahi format mein present karna ke liye bhi zaruri ho sakta hai.

## Kaise Karein:
Agar aapka string "hello world" hai, to aap use capital letters mein kaise convert kar sakte hain? Ye kaam asaan hai, bas kuch simple steps follow karein:

1. Sabse pehle, apne string ko variable mein store karein. Jaise ki:
```Swift
var myString = "hello world"
```

2. Ab ye string ko capital letters mein convert karne ke liye, `uppercased()` function ka use karein. Jaise ki:
```Swift
var myCapitalString = myString.uppercased()
```

3. Aur agar aapko string mein sirf first letter ko capital karna hai, to `capitalized` function ka use karein. Jaise ki:
```Swift
var myTitleString = myString.capitalized
```

4. Ye steps follow karne ke baad, aapko apne console mein output ke tarah capital letters mein string dikhega:
```
HELLO WORLD
Hello World
```

## Gehri Jaanch:
Swift mein dhyan deni wali choti si baat hai ki, agar aap original string ko capital letters mein convert karte hain, to wo string mutable alter hojati hai. Yani, agar aap use baad mein uppercase or lowercase mein use karna chahte hain, to aapko original string ko save karna hoga. Isse bachne ke liye, aap `lowercased()` function ka use karke uppercase characters ko lowercase mein convert kar sakte hain aur use save kar sakte hain. Isi tarah aap `uppercased()` function ka use karke lowercase characters ko uppercase mein convert kar sakte hain.

## Dekhein Bhi:
Agar aapko aur bhi Swift programming se jude articles padhne hain, to neeche diye gaye links zaroor check karein:
- [Strings in Swift](https://www.hackingwithswift.com/sixty/2/1/strings)
- [Working with Strings in Swift 5](https://medium.com/better-programming/working-with-strings-in-swift-5-f93ba6cc8e2f)
- [Understanding Mutability in Swift](https://www.iosapptemplates.com/blog/swift-programming/swift-mutability)