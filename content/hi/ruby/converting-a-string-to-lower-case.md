---
title:                "Ruby: स्ट्रिंग को लोअर केस में बदलना"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

##Kyu: 

Kisi bhi programming language mein, hum string manipulation se waqif hote hai. Kabhi kabhi, hume upper case ke saath problem ho sakti hai, jaise ki website URLs ko lower case mein convert karna ya fir passwords ko case-sensitive banane ke liye. Iska ek solution hai string ko lower case mein convert karne ka. Is blog post mein hum is process ko Ruby programming language ke through explore karenge.

##Kaise Kare:

```Ruby
puts "Hello World!".downcase
```

```
irb(main):001:0> puts "Hello World!".downcase
hello world!
=> nil
```

Jaisa ki humne dekha, `downcase` method humare string ko lower case mein convert kar deta hai. Isko hum variable mein bhi store kar sakte hai:

```Ruby
my_string = "Hindi Blog Post".downcase
puts my_string
```

```
irb(main):001:0> my_string = "Hindi Blog Post".downcase
=> "hindi blog post"
irb(main):002:0> puts my_string
hindi blog post
=> nil
```

Is tarah hum apne strings ko aasani se lower case mein convert kar sakte hai.

##Gehraai Mein Jaae:

Ruby mein `downcase` method ka use karne se pehle, hume kuch important baatein dhyaan mein rakhni chahiye. Sabse pehle, ye method sirf strings ko lower case mein convert kar sakta hai. Agar hum isko numbers, symbols ya spaces ke saath upyog karenge to iska koi effect nahi hoga. Isliye, humare string ke content ko dhyaan se dekhna hoga.

Ek aur important point hai ki `downcase` method original string ko change nahi karta hai. Balki woh ek naya string return karta hai. Isliye, dyan rahe original string ka value same hi rahega.

##Dekhiye Agar:

Agar aapko sorting ya searching ke liye lower case string chahiye, toh aap `downcase` method ka use kar sakte hai. Iske alawa, aap password security aur data validation mein bhi iska upyog kar sakte hai.

##Aur Padhiye:

- [Ruby String Documen