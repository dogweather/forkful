---
title:    "Bash: स्ट्रिंग्स का सम्मिलन"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Kyun
Bahut se log shabdon ko ek saath jodne ke liye string concatenation ka upyog karte hain. Yeh ek aasaan aur prabhavshali tareeka hai shabdon ko ek saath jodne ka jo prushtham aur kathin kaam ko bhi aasaan bana deta hai.

## Kaise Karein
Bash mein shabdon ko concatenation karne ke liye `+` operator ka upyog kiya jaata hai. Example ke liye, humne ek variable `name` mein apne naam ko store kiya hai aur ek variable `greeting` mein ek shubhakaamna ka shabd. Phir hum `+` operator ka upyog karke dono strings ko ek saath jod sakte hain aur output ko `echo` command se print kar sakte hain.

```Bash
name="John"
greeting="Namaste"
echo $greeting", "$name" Ji! Aap kaise ho?"
```

Output:
```Bash
Namaste, John Ji! Aap kaise ho?
```

Agar hum apne naam mein kuch aur changes karna chahte hain, jaise ki `name= "Smith"`, toh bhi output automatically update ho jaayega. Isse hume ek aasaan tareeka milta hai variables ke saath kaam karne ka.

## Gaharaai Mein Jaana
Bash mein string concatenation bahut hi powerful tool hai. Isse hum apne strings ko manipulate kar sakte hain aur unmein alag-alag variables ka upyog kar sakte hain. Isse programming mein flexibility aur efficiency badhti hai. Hum apne codes ko aur bhi dynamic aur scalable bana sakte hain.

Ek aur important point hai ki Bash mein hum shabdon ke alawa numbers ko bhi concatenate kar sakte hain. Yeh variable type dependent hota hai. Agar hum decimal numbers ko concatenate karenge, toh unka output string format mein hi aayega. Lekin agar hum integers ko concatenate karenge toh hume ek sum ka output milega.

## Dekhiye Bhi
Is article mein humne Bash mein string concatenate karna sikh liya hai. Ab aap is technique ka upyog karke apne codes ko aur bhi efficient aur dynamic bana sakte hain. Aap mein se kuch aur log bhi is technique ka upyog karke apne projects ko aur bhi behtar bana sakte hain.

Agar aapko Bash aur programming ke baare mein aur bhi jaankari chahiye, toh aap inn links ko check kar sakte hain:

- [Bash Documentation](https://www.gnu.org/software/bash/manual/) 
- [FreeCodeCamp Bash Tutorial](https://www.freecodecamp.org/news/the-linux-commands-handbook/#bash-concatel)
- [Bash Tutorials on Youtube](https://www.youtube.com/results?search_query=bash+tutorial)

Keep coding and keep learning!