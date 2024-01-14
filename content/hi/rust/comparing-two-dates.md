---
title:    "Rust: दो तारीखों की तुलना करना"
keywords: ["Rust"]
---

{{< edit_this_page >}}

# Kyun

 Kabhi kabhi hamare paas do dates hote hain aur hamein unhe compare karna hai, jaise ki ek event ki date aur current date. Is samasya ka samadhan karne ke liye hamko do dates ko compare karne ki jarurat hoti hai.

# Kaise Karein

```Rust
fn main() {
    use chrono::{DateTime, Utc};

    let event_date = "2021-10-10".parse::<DateTime<Utc>>().unwrap();
    let current_date = Utc::now();

    if event_date > current_date {
        println!("Event is yet to happen");
    } else if event_date == current_date {
        println!("Event is happening right now");
    } else {
        println!("Event has already happened");
    }
}
```

Is code block mein, humne rust programming language ka use karke do dates ko compare kiya hai. Humne `chrono` aur `Utc` library ka use kiya hain jo dates ko handle karne ke liye commonly use hota hai. Hamne do dates ko `event_date` aur `current_date` variable mein store kiya hai. Fir humne `if statement` ka use karke unhe compare kiya hai aur uske hisaab se kuch output print kiya hai. Is tarah se hum dates ko compare kar sakte hain.

# Gehri Jankari

Dates ko compare karne ke liye, hamein unhe ek common format mein convert karna hota hai. Isliye humne `parse()` method ka use kiya hai jisse hum dates ko string se `DateTime` object mein convert kar sakte hain. Fir hum `>`, `==` aur `<` operators ka use karke unhe compare karte hain.

# Dekhiye Bhi

- Rust programming language ka official documentation: https://www.rust-lang.org/learn
- Chrono library ka documentation: https://docs.rs/chrono/0.4.19/chrono/
- Utc library ka documentation: https://docs.rs/chrono-tz/0.5.2/chrono_tz/