---
title:                "Gleam: एक पाठ फ़ाइल पढ़ना"
simple_title:         "एक पाठ फ़ाइल पढ़ना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Kyun:
Kisi ko ek text file ko padhne mein kyun ruchi hai, iske baare mein 1-2 vakyon mein vivaran diya gaya hai.

## Kaise Karein:
"```Gleam ... ```" code blocks ke andar coding udaharan aur sample output. 
Jaise ki hum sabhi jaante hain, text files humein data ko store karne aur access karne ka ek sadhan pradaan karte hain. Lekin kya aapne kabhi socha hai ki uss text file ke andar ka data kaise padha jaata hai? Aaj hum Gleam programming language ke madhyam se ye seekhenge.

Sabse pehle, humein ek text file banani hogi jiska naam "sample.txt" hoga aur jo humaare current working directory (cwd) mein hoga. Iske liye hum "create_sample_txt" naam ka function bana sakte hain, jis mein hum ye code likh sakte hain:

```Gleam
fn create_sample_txt () {
  let file = File.open("sample.txt", [:create])
  file.write("This is a sample text file.")
  file.close()
}
```

Iske baad hum ye function humaare main function mein call karenge, jiske liye hum ye code likh sakte hain:

```Gleam
fn main () {
  create_sample_txt()
}
```

Ye code humaare program ko compile aur execute karne se pehle humaare create_sample_txt() function ko call karega aur ye sample.txt file humaare cwd mein create karega. Ab hum ye file ke andar ka data kaise padh sakte hain, iske baare mein jaanenge.

```Gleam
fn main () {
  let file = File.open("sample.txt", [:read])
  let contents = file.read_to_string()
  file.close()
  Debug.print(contents)
}
```

Is code mein humne file.open() function ka ek aur parameter [:read] pass kiya hai jisse ye specify ho jaata hai ki hum iss file ko read mode mein open karna chahte hain. Uske baad humne read_to_string() function ka use kiya hai jo humaare file ke andar ka data ek string mein return karega. Aur finally, humne debug.print() function ka use kiya hai string ko print karne ke liye. Ab humaare output mein "This is a sample text file." string dekh sakte hain.

## Gehri Jhaank:
Text file ko padhne ke liye humne file.open() function mein [:read] parameter ka use kiya, lekin iss parameter mein hum aur bhi options specify kar sakte hain jaise ki [:read, :write, :append] aur [:create]. Agar hum file ko sirf read mode mein open karna chahte hain toh [:read] hi specify karna hoga.

Iske alawa, hum file.read() function ka bhi use kar sakte hain jiske alawa aur bhi options available hain jaise ki read_to_string(), read_to_bytes(), read_line(), etc. In functions mein se har ek ki apni alag syntax hoti hai, isliye jo bhi data hum file se read kar rahe hain, uske hisaab se hum sahi function ka use karna hoga.

See Also:
- [Gleam File Module Documentation](https://gleam.run/modules/file/)
- [Introduction to Gleam Programming Language](https://gleam.run/getting-started/intro/)