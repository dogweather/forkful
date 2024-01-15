---
title:                "फ़ोल्डर का अस्तित्व जांचें"
html_title:           "Swift: फ़ोल्डर का अस्तित्व जांचें"
simple_title:         "फ़ोल्डर का अस्तित्व जांचें"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Kyun

Kisi bhi vyakti ke liye zaroori hai ki ve ek directory ke hote hue hai ya nahi, kyunki yeh unko apne code mein sahi tarah ke changes karne ke liye pata chal jaata hai. Iske alawa, yeh bhi zaroori hai agar hum kisi specific directory mein kuchh files ko search karna chahte hain.

## Kaise Kare

```Swift 
let fileManager = FileManager.default
let path = "/Users/username/directory_name"

if fileManager.fileExists(atPath: path) {  
    print("Directory exists")
} else {  
    print("Directory does not exist")
}
```

Is code snippet mein, humne `FileManager` ka default instance banaya hai aur path ka ek string define kiya hai jis tarah se humare system mein directory exist karta hai. Fir humne `fileExists(atPath:)` ka method use kiya hai jisse hume pata chalta hai ki kya humare system mein woh directory hai ya nahi. Agar hai, toh "Directory exists" print karega, nahi toh "Directory does not exist" print karega.

## Deep Dive

Directory ka existence check karne ke liye, hum `fileExists(atPath:)` method ka use karte hain. Yeh method hume path ko specify karne ke liye use hota hai aur iska return value boolean hota hai. Agar directory exist karta hai, toh return value `true` hota hai, nahi toh `false`. Is method ka use bilkul waise hi hota hai jaise ki hum file ko check karte hain, lekin hum directory ki jagah file ka path define karte hain.

## Dekhein Bhi

- [FileManager - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/filemanager)
- [Checking for the Existence of a File - Swift by Sundell](https://www.swiftbysundell.com/basics/file-exists-checking/)
- [How to Check if a File Exists in Swift - Hacking with Swift](https://www.hackingwithswift.com/example-code/system/how-to-check-if-a-file-exists-using-filemanager)