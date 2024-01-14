---
title:                "Ruby: दो तारीखों की तुलना"
simple_title:         "दो तारीखों की तुलना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

### Kyon:
Date comparison ek aham kaam hai jo Ruby programmers ke liye bahut zaroori hai. Isse aap apne code mein tithiyon aur samay ki tulna karke, unke beech ki farak aur similarities ko pata kar sakte hain. Isse aap apne code ko control aur organize kar sakte hain.

Is blog post mein hum aapko date comparison ki zaroorat ki wajah aur usse kaise deal karein, dono ka solution dene wale hain. Hum aapko code snippets aur example output dikhayenge jisse aap aaram se samajh sakte hain ki dates ko compare karna kitna aasan hai.

Isliye agle paarag se hum shuru karte hain!

### Kaise Karein:
Ruby mein tithiyon aur samay ko compare karne ke liye aapko "Date" aur "Time" class ka use karna hoga. Ye built-in classes hain aur aapko kuch special methods provide karte hain jaise ki `before`, `after`, `equal?` aur `between?`.

Agar hum do dates ko compare karna chahte hain, to hum pehle `Date` ya `Time` class se unhe alag variable mein store karenge. Iske baad hum comparison method ka use karenge aur result ko output ke roop mein print karenge.

Jaise ki,

```
Date1 = Date.new(2021, 6, 15)
Date2 = Date.new(2021, 6, 20)

puts Date1.equal?(Date2) # returns false
puts Date1.after?(Date2) # returns false
puts Date1.before?(Date2) # returns true
```

Is code mein humne `equal?`, `after?` aur `before?` method ka use kiya hai apni dates ko compare karne ke liye. Aap isme alag-alag dates ko bhi use kar sakte hain aur result check kar sakte hain.

Agar hum samay ko compare karna chahte hain, to hum `Time` class ka use karenge. Iske liye bhi same methods use honge like `equal?`, `after?`, `before?`.

Jaise ki,

```
Time1 = Time.new(2021, 6, 15, 12, 30, 0)
Time2 = Time.new(2021, 6, 15, 15, 30, 0)

puts Time1.equal?(Time2) # returns false
puts Time1.after?(Time2) # returns false
puts Time1.before?(Time2) # returns true
```

### Deep Dive:
Ab hum dekhenge ki kaise hum dates ki alag-alag tarah se compare kar sakte hain. Ruby mein tithiyon aur samay ko compare karne ke liye aap "datetime" library ka bhi use kar sakte hain. Ye library aapko aur bhi advanced methods provide karti hai jaise ki `compare_by` aur `compare_to`.

Aap humare next blog post mein in sabhi methods aur techniques ke baare mein detail mein padh sakte hain. Isse aap apne code mein aur bhi accurate date comparison implement kar sakte hain.

### Dekhein Bhi:
- [Ruby Date class documentation](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html)
- [Ruby Time class documentation](https://ruby-doc.org/stdlib-3.0.0/libdoc/time/rdoc/Time.html)
- [Ruby datetime library documentation](https://rubygems.org/gems/datetime/versions/2.0.5)