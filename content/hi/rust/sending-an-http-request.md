---
title:                "एक HTTP अनुरोध भेजना"
html_title:           "Rust: एक HTTP अनुरोध भेजना"
simple_title:         "एक HTTP अनुरोध भेजना"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

##
Kya hai aur kyu?: 
HTTP request bhejna ek prakar ka communication hai jisme server se client ko data ya information bheja jaata hai. Iss prakriya ka upyog website par content ko access karne ke liye kiya jaata hai. Programmers isliye iska upyog karte hai kyunki isse communication ko asaani se handle kiya ja sakta hai aur server se information ko retrieve kiya ja sakta hai.

Kaise karein:
```Rust
fn main() {
    reqwest::get("https://example.com")
        .send()
        .expect("Could not send request");
}
```

Deeper Dive: 
HTTP request ko bhejne ka prathamik tarika GET request tha, jisme client se server tak information jaata tha. Lekin ab POST, PUT, DELETE jaise methods bhi use kiye jaate hain. Rust mein, reqwest library ka upyog karke HTTP request bheja ja sakta hai. Iske alawa, hyper aur curl jaisi libraries bhi kaafi prachalit hain.

See Also:
- https://doc.rust-lang.org/stable/book/ch12-01-accepting-command-line-arguments.html
- https://learning-rust.github.io/docs/e6.http_server.php