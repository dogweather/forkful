---
title:    "Rust: संयोजित प्राकृतिक संख्याएं"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Kyun

Random numbers ka vyavhar karne ke liye kyun kisi ko uttejit karna chahiye, iske pichhe kya hai? Random numbers jode hue data ki vyakulta aur avyakta ko represent karte hain. Is liye, inse chunke data ko apne hisab se organize kar sakte hain, isse hamare code kaafi versatile ho jaate hain. Aur agar hamari software ke liye secure passwords banane ki zaroorat hai, toh random numbers bahut zaroori hote hain.

# Kaise Karein

Kam karein, sunheri aankhein karein - yeh hona chahiye apke dimag mein agar apne shuruat mein naam nahin paaya Rust ke sath. Ho na sake, langauge ke saathe thoda der lag sakta hai, par is language ke madhyam mein, 	random numbers kaise ban sakta hai yeh seekhna zaroori hai. 

```Rust
// Random numbers ka example
use rand::Rng;
// Use karne wala path
fn main() {
    // Output mein 5 random numbers print karaye
    println!("Random Numbers: ");
    for _ in 0..5 {
        println!("{}", rand::thread_rng().gen_range(0, 10));
        // 0 se lekar 10 ke beech mein random numbers generate karein 
    }
}
```

Is code snippet mein, `rand` crate ka istemaal kiya gaya hai random numbers banane ke liye. Pehle `use` line mein, `rand::Rng` ko use kar ke, crate ko import kiya gaya hai. Fir, `main` function mein, `println` function ka istemaal kiya gaya hai random numbers print karne ke liye. Is mein `for` loop ka upayog kiya gaya hai, jisme har baar `rand::thread_rng().gen_range(0, 10)` call kiya jata hai, jisse humare program ko random numbers generate karne ke liye kaha jata hai.

# Gahra Sukhchaar

Random numbers generate karne ki peechhe, Rust ke `rand` crate ke alawa bahut se techniques dheere dheere develop huye gaye hain. Chuki random numbers ko secure banana bahut zaroori hai, is liye kai techniques hain jaise PRNGs (Pseudorandom number generators), CSPRNGs (Cryptographically secure pseudorandom number generators) aur entropy based random number generators. In sabhi techniques mein, randomness aur unpredictability ka balance dhyaan mein rakhna zaroori hai taki koi bhi hacker is randomness ko predict na kar sake.

# Dekho Bhi

Random numbers generate karne ke alawa, Rust mein aur bhi bahut se interesting features hain. Agar aapko is language ke bare mein aur jaankari chaiye, toh neeche diye gaye links check karein:

- [Rust Official Website](https://www.rust-lang.org/)
- [Rust Tutorial in Hindi](https://github.com/w3hexschool/rust-tutorials-in-hindi)
- [Rust Programming - Full Course](https://www.youtube.com/watch?v=zF34dRivLOw)