---
title:                "दो तारीखों का तुलना करना"
html_title:           "Gleam: दो तारीखों का तुलना करना"
simple_title:         "दो तारीखों का तुलना करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

##Why
Mai samajh sakta hoon, aap yahan kyun hain. Aapne date ke samne or peeche cheezon ko compare karne ki sochi hai. Shayad aap koi event plan kar rahe ho ya fir ticket booking karne ke liye date check kar rahe ho. Chahe aap koi practical reason ho ya sirf curious hain, Gleam aapke liye sahi jagah hai date comparison ke liye.

##How To
Sabse pehle, hamein do dates ke beech mein difference (antar) nikalna hai. For example, agar humein pata karna hai ki aaj kitne din bacche hain December 31, 2021 se, toh humein sirf "December 31, 2021" ko subtract karna hai aaj ki date se. Yeh kuch is tarah se dikhega:

```Gleam
  let aajKaDin = Date.today()
  let eventKaDin = Date.from_date({year: 2021, month: 12, day: 31 })
  let difference = eventKaDin - aajKaDin
```

Is code se humein difference (antar) 244 aayega, jo ki humare liye number of days batata hai. Aap is technique ko kisi bhi date comparison ke liye use kar sakte hain, jaise ki booking dates, project deadlines, etc.

##Deep Dive
Date comparison ke liye, Gleam mein "Date" module available hai. Ismein aapko date aur time ko manipulate karne ke liye functions milenge. For example, aap "is_after" function ka use karke check kar sakte hain ki kya ek date dusre date ke baad hai. Yeh kuch is tarah se dikhega:

```Gleam
  let aajKiDate = Date.today()
  let kalKiDate = Date.tomorrow()

  if Date.is_after(aajKiDate, kalKiDate) {
    io.print("Aaj ki date kal ki date ke baad hai.")
  } else {
    io.print("Aaj ki date kal ki date se pehle hai.")
  }
```

Is code mein humne "is_after" function ka use kiya hai, jo ki humein True ya False return karta hai. Aap "is_before" function ko bhi use kar sakte hain ek date dusre date se pehle hai ya baad hai check karne ke liye.

##See Also
Agar aapko Gleam aur date comparisons ke baare mein aur jaankari chahiye, toh aap neeche diye gaye links ko check kar sakte hain:

- [Gleam official documentation](https://gleam.run/documentation)
- [Hindi tutorial on Gleam](https://gleam.run/tutorials/hindi)
- [Article on date and time in Gleam](https://dev.to/marouenrg/date-and-time-in-gleam-26j7)