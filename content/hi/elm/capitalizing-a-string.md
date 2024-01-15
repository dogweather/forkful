---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
html_title:           "Elm: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Kyun
Kisi bhi string ko capitalization karne ka karan ho sakta hai ki aap usko padhne ya dikhne me asani chahte hai, ya fir use ek specific format me store karna chahte hai. Elm me string ko capitalization karne ka sahi aur saral tarika hai, jise hum is article me samjhenge.

## Kaise Kare
```Elm
import String

capitalizedString = String.toUpper "yeh ek string hai"
-- "YEH EK STRING HAI"
```

Is code snippet me humne `String` module ko import kiya aur `String.toUpper` function ka use kiya hai jo hume di gayi string ko capital letters me convert karega. Is code me `capitalizedString` variable me ab converted string ka value store hoga. Aapka converted string variable ka naam aur string aapke requirement ke according alag ho sakta hai. 

Agar aap string ko sirf pehle character ko capital karna chahte hai, to uske liye aap `String.toTitle` function ka use kar sakte hai.

```Elm
import String

capitalizedString = String.toTitle "yeh ek string hai"
-- "Yeh ek string hai"
```

## Deeper Dive
Elm me string ko capitalization karne ke liye `String` module ke alawa aur bhi modules aur functions available hai. Aap `String.toLower`, `String.toUpperFirst` aur `String.toTitleWords` functions ka bhi use kar sakte hai. In functions ko use karne ke liye aapko `import` statement ki madad se `String` module ko import karna hoga.

## Dekhenge
Agar aap aur Elm programming ke bare me jaanna chahte hai, to aap in links ko dekh sakte hai:
- [Elm Documentation](https://guide.elm-lang.org/)
- [Elm in Hindi](https://dev.to/wijngaat/exploring-elm-5ia6)