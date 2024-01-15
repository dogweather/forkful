---
title:                "एक टेक्स्ट फाइल लिखना"
html_title:           "Ruby: एक टेक्स्ट फाइल लिखना"
simple_title:         "एक टेक्स्ट फाइल लिखना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Kyaorama
Abhinav Topedar, 
Ek text file likhneka kya fayda hota hai? Shayad aapko lage ki koi app aapki diary likhna hai parantu agara aap programming mein pravesh karna chahate hai na, to text file likhna ek aadhunik tarika hai. Technique ka matlab hai ki aap apne thode samay mein note karsakte hain apne code mein aur test karsakte hain saamne laya respect kuch samay baad. Ye ek aasaan aur bharoseemand tarika hai apne progress ko track karne ka.

## Kaise Kare
Aapko Ruby ka instalation jaruri hai aapki system par. Jab aap apne terminal mein Ruby command likhenge, aapko IRB prompt screen mein dikhega. Lekin abhi hum text file likhege. Chaliye, ye ek simple Ruby code example ka padhna shuru kare jismein hum ek text file likhege aur uski output ko print karege:
```ruby
# Create a new text file and open it
file = File.new("new_file.txt", "w")

# Add some text to the file
file.puts "Welcome to my first text file!"
file.puts "I am learning Ruby and it's awesome!"

# Close the file
file.close

# Open the file in read mode and print the contents
puts "The file contains:"
puts File.read("new_file.txt")
```
Jaise aapko dikh raha hoga, humne `File.new` ke dwara ek naya text file create kiya aur `file.puts` ke dwara usmein content add kiya. Fir `close` ke dwara file ko band kiya aur `File.read` se file ka content print kiya. Chaliye ab is code ko chalakar dekhe:
```
The file contains:
Welcome to my first text file!
I am learning Ruby and it's awesome!
```

## Gehri Jhanki
Likhne ke doran, `file.puts` ka istemal hota hai content ko file mein likhne ke liye. Hum `w` ka istemal karte hain `File.new` mein text file ko likhne ke liye. Humein `close` ke dwara file ko band karna jaruri hai taki humare changes file mein save ho. Agar hum `close` na kare, toh humare changes file mein save nahi honge. Humein `File.read` se file ko read karna padega taki hum file ka content print kar sake.

## Dekhe bhiay
Agar aap aur bhi Ruby sikhna chahate hain, toh niche diye gaye links check kare:
- [RubyKoariya](https://rubymonk.com/) Ek interactive platform jaha aap Ruby ko seekh sakte hain.
- [RubyKoariya pane ke liye Ruby installer](https://www.ruby-lang.org/en/documentation/installation/) Installation guide aur steps ke liye.
- [The Ruby language](https://www.ruby-lang.org/) Official website jaha se aap latest updates aur documentation dekh sakte hain.

## Dekh na Bhai
Jab bhi aap kuch programming seekhte hain, ek text file likhna ek bharoseemand aur zaroori technique hai. Ruby mein, ye kaam bohot hi aasaan hai. Abhi aap gaye! Happy coding!