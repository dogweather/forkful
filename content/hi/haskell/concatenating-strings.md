---
title:                "स्ट्रिंग्स जोड़ना"
html_title:           "Haskell: स्ट्रिंग्स जोड़ना"
simple_title:         "स्ट्रिंग्स जोड़ना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Kyon
Concatenating strings kyon karne ka ek simple tareeka hai apne code mein text ko combine karne ka. Jaise ki "Hello" aur "world" ko combine karne se "Hello world" banta hai. Isse apke code mein dynamic aur useful text generate karne mein madad milti hai.

## Kaise Kare
Code karne ke liye sabse pehle aapko ```Haskell``` language ki basic knowledge honi chahiye. Agar aap beginner hai toh pehle ```Haskell``` ke basic concepts aur syntax ko samjhe. Iske baad aap concatenation ke liye ```++``` operator ka use kar sakte hai.

### Simple Concatenation

```Haskell
main= do
    let hello = "Hello"
        world = "world"
    putStrLn (hello ++ " " ++world)

```
Output:
```
Hello world
```

Is example mein humne ```let``` statement ka use kiya hai jisse humne variables ```hello``` aur ```world``` ko define kiya. Fir humne ```putStrLn``` function ke andar yeh variables concatenate karke print kiya.

### String Interpolation

```Haskell
main= do
    let num = 10
    putStrLn ("The number is: " ++ show num)

```
Output:
```
The number is: 10
```

Is example mein humne ek variable ```num``` ko integer value assign kiya hai. Fir humne use ```++``` operator ki madad se string ke sath concatenate kiya aur ```show``` function ka use kiya jisse hum integer ko string mein convert kar sake.

## Gehri Jankari
Concatenation mein hum ```++``` operator ka istemal karte hai jisse hum ek se jyada strings ko combine kar sakte hai. Ismein arrays bhi concatenate kar sakte hai jisse hum ek saath multiple strings ko print kar sake.

Concatenation ko use karne se pehle humein samajhna jaruri hai ki humne sahi tarike se strings ko define kiya hai. Float ya integer values ko string ke sath concatenate karne se error aata hai. Iske alawa hum ```concat``` function ka bhi use kar sakte hai jo multiple strings ko concatenate kar deta hai.

## Dekhen Bhi
- ["Introduction to Haskell" by Learn You a Haskell]: https://learnyouahaskell.com/introduction
- ["Haskell for Beginners" by Haskell.org]: https://www.haskell.org/documentation/
- ["Haskell Tutorial" by TutorialsPoint]: https://www.tutorialspoint.com/haskell/