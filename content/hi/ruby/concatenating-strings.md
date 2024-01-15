---
title:                "स्ट्रिंग को जोड़ना"
html_title:           "Ruby: स्ट्रिंग को जोड़ना"
simple_title:         "स्ट्रिंग को जोड़ना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Agar aapne kabhi bhi Ruby programming language ke baare mein suna hai ya kisi experienced programmer se baat ki hai, toh aapko pata hoga ki isme "string concatenation" ek bahut important concept hai. Isse aap apne programs mein strings ko jod kar alag-alag words aur sentences bana sakte hain. Yeh ek bahut useful technique hai jo aapke coding skills ko improve karti hai.

## How To

Iske liye sabse pehle aapko Ruby language ko achi tarah se samajhna hoga. Uske baad aapke paas kuch options hain strings ko concatenate karne ke liye. Yeh options aapko "```+```" plus operator aur "```<<```" concatenation assignment operator ke through dikhenge. Ab dekhte hain kaise hum inka use karein:

```Ruby
# Plus operator ka use karke string concatenate karna:

puts "Hello" + " " + "World"  
# Output: Hello World 

# Concatenation assignment operator ka use karke string concatenate karna:

name = "Hindi"
name << " is a beautiful language."
puts name
# Output: Hindi is a beautiful language.
```

Yeh coding examples mein aapne dekha ki hum "```+```" operator aur "```<<```" operator ka use karke strings ko concatenate kar sakte hain. Aapke paas aur bhi options hain jaise "```concat()```" method aur "```interpolation```" method, jo aap apni research kar sakte hain.

## Deep Dive

Yeh important hai ki aap Ruby language mein strings ko concatenate karte waqt kaise spaces ka use karte hain. Agar aapka program mein spaces nahi hain toh aapke output mein words aur sentences sahi se display nahi honge. Iske liye hum quotes ya phir interpolation method ka use kar sakte hain. Iske alawa, agar aap multiple variables ko concatenate karte hain, toh unmein se kisi bhi variable mein jitne bhi spaces honge, unhe output mein bhi include kiya jayega.

## See Also
[Official Ruby Language Documentation](https://ruby-lang.org)

[Ruby Beginner's Guide](https://www.rubyguides.com)

[Ruby String Methods](https://www.rubyguides.com/2018/04/ruby-string-methods/)