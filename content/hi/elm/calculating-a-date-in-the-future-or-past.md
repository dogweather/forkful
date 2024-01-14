---
title:                "Elm: भविष्य या भूतकाल में दिनांक का गणना"
simple_title:         "भविष्य या भूतकाल में दिनांक का गणना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Kyu

Din aur mahine ki tarikh ka kabha kabhi istemaal karna kyu jaruri hota hai, uske liye sirf 1-2 sentence ki samajh hoti hai.

## Kaise karein

Agar aapko hogi kabhi bhi future aur past mei tarikh nikalne ki jarurat, aapko kin mathametical calculation ki jarurat padegi. Elm programming language ke through hum aasani se tarikh ki calculation kar sakte hain. Dekhiye neeche diye gaye code blocks aur unki output ko.

```Elm
-- Future date calculation
calculateFutureDate : Int -> Int -> Int -> Date
calculateFutureDate day month year =
    Date.fromParts year month day

-- Sample output:
calculateFutureDate 25 12 2021 --> "2021-12-25"

-- Past date calculation
calculatePastDate : Int -> Int -> Int -> Date
calculatePastDate day month year =
    Date.fromParts year month day

-- Sample output:
calculatePastDate 15 6 1999 --> "1999-06-15"
```

## Gehraai mein

Tarikh ki calculation mei sabse important hai ki hum sabse pehle Date library ko import karein apne project mei. Date library hume future aur past ki calculation ke liye sahi tareeke se date ko manage karne mei help karega.

Jab bhi hum future ya past date calculate karte hain, hume day, month aur year ki value pass karni hoti hai calculateFutureDate() aur calculatePastDate() functions mei. Aur yadi hume specific time bhi add karna ho toh hum Date timezone aur Time modules ka use kar sakte hain. Elm programming language ki is vidhi ko follow karke aap kabhi bhi future aur past date ko asani se manage kar sakte hain.

## See Also

- https://package.elm-lang.org/packages/elm/time/latest/Time
- https://package.elm-lang.org/packages/elm-community/date-extra/latest/
- https://elmprogramming.com/guides/dates.html