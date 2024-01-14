---
title:    "Haskell: तारीख को प्राप्त करना"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Kyun
Aaj ke samay mein, computer programming ek bahut hi important aur prabhavshali field ban gaya hai. Aur Haskell, ek functional programming language, ek popular option hai is field mein. Agar aap bhi ek Hindi readers hain aur haskell ka istemal karte hain, toh aapko current date ko jaanne ki zaroorat hogi. Is blog post mein hum jaanenge ki current date kyun zaroori hai aur isko kaise hum Haskell mein pa sakte hain.

## Kaise Karein
Current date ko Haskell mein paane ke liye, hum `getClockTime` aur `toUTCTime` functions ka istemal karenge. Issey hum current date ko seconds mein paa sakte hain.

```
import Data.Time.Clock

main = do
    cTime <- getClockTime
    let utcTime = toUTCTime cTime
    print utcTime
```

Is code mein, hum `Data.Time.Clock` library ko import karte hain aur `getClockTime` function ka istemal kar current system time ko retrieve karte hain. Phir, `toUTCTime` function ko use karke hum is time ko UTC format mein convert karte hain. Phir end mein, hum `print` function ka istemal kar current date ko output karte hain.

Is code ka output neeche diya gaya hai:
```
2021-05-20 05:15:28.103353442 UTC
```

Ab hum current date ko alag-alag formats mein bhi retrieve kar sakte hain. Iske liye hum `Data.Time.Format` library ka istemal karenge. Neeche di gayi code mein hum current date ko dd-mm-yyyy format mein lekar aayenge.

```
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime

main = do
    cTime <- getZonedTime
    let formattedDate = formatTime defaultTimeLocale "%d-%m-%Y" cTime
    print formattedDate
```

Is code ka output neeche diya gaya hai:
```
20-05-2021
```

## Gehri Jhaank
Current date ko retrieve karna bahut simple hai, lekin uske piche ka concept thoda gehra hai. Haskell mein, hum `IO` monad ka istemal karte hain jab hum kisi external resource, jaise system time, ko access karte hain. Isi liye humne `do` keyword ka istemal kiya `main` function ke andar `getClockTime` function ko call karne se pehle. Phir hum `let` keyword ka istemal kiya variable assign karne ke liye.

Agar hum `do` keyword ka istemal na karein, toh humein ek error milegi ki humein `IO` monad mein hain aur humein use karna hoga.

## Dekhein Bhi
Agar aapko current date ke alawa bhi advanced topics aur concepts ke baare mein jaanna hai, toh aap neeche diye gaye links ko check kar sakte hain:

- [Yeh q kyu hua? Haskell aur ntroduction to IO](https://www.youtube.com/watch?v=KZr3387p76Q)
- [IO Monads in Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghci/handling-io)
- [IO Monads in Haskell Wiki](https://wiki.haskell.org/Introduction_to_IO)
- [Haskell's type system in a nutshell](https://www.reddit.com/r/haskell/comments/cludut/haskell_type_system_in_a_nutshell/)
- [Current System Time in Clojure](https://stackoverflow.com/a/721581/15074122)
- [DateTime Module in Python](https://docs.python.org/3/library/datetime.html)

Aasha karte hain ki aapko yeh blog post samajh aaya hoga aur ab aap current date ko Haskell mein retrieve kar sakte hain. Happy coding!