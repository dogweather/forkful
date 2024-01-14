---
title:    "Gleam: कम्प्यूटर प्रोग्रामिंग पर एक लेख का शीर्षक: कमांड लाइन आर्ग्यूमेंट्स पढ़ना"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

"## Kyun: Gleam mein command line arguments ko padhne ke liye kyon kisi ki dilchaspi hai?

Jab hum kisi program ko command line se run karte hain, tab hamein command line arguments ki zaroorat padti hai. Ye hume program mein dynamic input deta hai jo ki humare code ko functional banata hai. Isme hum apne program ko flexible banate hain aur usme changes kar sakte hain bina code ko compile kiye. Ye bahut hi faydemand ho sakta hai jab hum multiple environments mein apna code run karna chahte hain.

"## Kaise karein: Command line arguments ko kaise padhen Gleam mein?

Command line arguments ko padhna bahut hi aasan hai Gleam mein. Sabse pehle aapko `gleam/io` package ko import karna hoga. Fir aap `gleam/io` package mein diye gaye `argv()` function se command line arguments ko padh sakte hain. Iske liye aapko ek `List(String)` return hoga jisme saare arguments honge. Neeche diye gaye example code se aapko samajh aa jayega.

```Gleam
import gleam/io

fn main() {
  io.argv() |> io.print
}
```

Is code ka output hume command line arguments ki list dega. Agar hum ise run karein to is tarah se output aayega:

```Gleam
> gleam run app.gleam arg1 arg2
[arg1, arg2]
```

Is tarah se aap apne program mein arguments ko padh sakte hain aur unko use kar sakte hain apne code ko functional aur dynamic banane ke liye.

"## Gehri Jaanch: Command line arguments ko padhne ki aur gehri jaankari

Command line arguments padhna na sirf aapko code ko functional banane mein madad karta hai balki isse aap apne program ko bahut versatile bhi bana sakte hain. Isme aap apne code mein default values bhi set kar sakte hain jisse agar user koi argument na de to bhi program run ho sake.

Saath hi, command line arguments ke saath saath flags bhi use kar sakte hain jo ki humare program ke options hote hain. Aur jab hum in flags ko use karte hain to hume unki value bhi command line se read karni padti hai. Is tarah se hum apne program ko even more dynamic bana sakte hain.

"## Dekhein Bhi: Iske alawa aur jaankari ke liye

Is article mein humne command line arguments ko padhna aur use karna sikh liya hai. Agar aap aur bhi Gleam programming ke bare mein jaanna chahte hain, to neeche diye gaye links se aap humare dusre articles ko padh sakte hain:

- [Gleam: Functional Programming ki Roshni Mein](https://futurestud.io/tutorials/gleam-functional-programming-seekho)
- [Gleam: Concurrent Programming aur Erlang VM](https://futurestud.io/tutorials/gleam-concurrent-programming-seekho)
- [Gleam: Github Repo aur Documentation](https://github.com/gleam-lang/gleam#readme)

Yeh articles aapke Gleam programming journey mein aapki madad karenge. Happy coding! "