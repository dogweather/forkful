---
title:                "C++: दिशा-वर्णन से मेल खाने वाले अक्षरों को हटाना"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

"Kyun: Kuch log sochte hain ki unke code mein kuch anokhi prashani hai jahan woh kuch special characters ko hatana chahte hain. Is blog post mein hum yeh sikhenge ki kyun kisi bhi pattern ke matches ko delete karna zaruri ho sakta hai."

## Kyun 

Kisi bhi coding project mein, kai baar humein kuch special characters ko delete karna hota hai jaise ki whitespace, punctuation marks ya fir specific characters jo ki unwanted ho sakte hain. Isse hamare code ko efficient aur readable banane mein madad milti hai.

## Kaise Karein

Agar hum C++ programming language mein baat karein, toh hum 'erase' aur 'remove_if' functions ka istemal kar sakte hain jo ki standard template library mein available hain. 

```
// Erase function
string str = "Hello World!";
str.erase(remove(str.begin(), str.end(), 'l'), str.end());
cout << str << endl;

// Output: Heo Word!

// Remove_if function
string str = "Hello World!";
str.erase(remove_if(str.begin(), str.end(), ::ispunct), str.end());
cout << str << endl;

// Output: HelloWorld
```

Jaise ki hum dekh sakte hain, humne is code mein 'l' aur punctuations ko delete kiya hai, jisse hamara string puri tarah se clean ho gaya hai.

## Gehri Jankari

Kabhi kabhi humein specific pattern ke matches ko delete karna hota hai, jaise ki aise words jo ki uppercase letters se shuru ho rahe hain ya fir kisi particular number ko included kar rahe hain. Isme hum ek special function, 'remove_if' ka istemal kar sakte hain jahan hum apne according character ko delete ya replace kar sakte hain.

```
// Removing words starting with uppercase
string str = "This IS a TEST string";
str.erase(remove_if(str.begin(), str.end(), ::isupper), str.end());
cout << str << endl;

// Output: his a string

// Removing specific numbers
string str = "12203152021";
erase(remove_if(str.begin(), str.end(), [](char c){ return c == '2'; }), str.end());
cout << str << endl;

// Output: 1315201
```

Is tarah se hum 'remove_if' function ka istemal karke apne coding projects mein characters matching a pattern ko delete kar sakte hain.

## Dekhein Bhi

Agar aapko yeh article pasand aaya ho aur aapko aur bhi C++ programming se judi tips aur tricks jaan na ho, toh aap neeche diye gaye links ko check kar sakte hain:

- Link 1
- Link 2
- Link 3