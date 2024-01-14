---
title:    "C++: प्रोग्रामिंग में टेस्ट लिखना"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Kyun
Test likhne ka karan hai ki yeh aapki code ki functionality aur reliability ko sudharne mein madad karta hai. Test likhne se aapko bugs ke pata lagne, unhe fix karne, aur future ke changes ko aasani se handle karne mein madad milti hai.

## Kaise Kare
Test likhne ke liye, aapke paas ek testing framework hona chahiye, jaise ki Google Test ya Boost.Test. Yeh framework aapko test case banane ki suvidha pradan karta hai. Test case ka matlab hai ki aap apne code mein ek particular scenario ko simulate karte hai aur expect kiya jaata hai ki code sahi tarah se chalega. Upar se, aapko assertion bhi honge, jo ki code ke result ke saath expect kiya jaata hai.

Ek example ke roop mein, agar hum ek function likh rahe hai jo do numbers ko add kare, to hum yeh test case likh sakte hai:
```C++
TEST(AddTest, TwoPositive){
    ASSERT_EQ(5,add(2,3));
}
```
Iss test case mein, hum expect karte hai ki add function 2 aur 3 ko add karke 5 return kare. Agar yeh assertion pass nahi hua, toh humein pata chalega ki add function mein koi issue hai aur hum use fix kar sakte hai.

## Gehri Jhaank
Test likhne se aap apne code mein confidence bana sakte hai. Agar aap ek project teams ke saath kaam kar rahe hai, toh test likhna collaboration ko bhi improve karta hai. Har bar code mein kuch changes karne se pehle, aap test run kar sakte hai aur dekh sakte hai ki kuch bhi introduce kiya gaya bug toh nahi hai. Aur agar hai toh, usse aasani se fix kar sakte hai.

Test likhna aapke code ko maintainable bhi banata hai kyunki future mein bhi aapko pata hai ki aapke code mein kya changes huye hai aur kaise impact honge aapke existing tests par.

## See Also
- [Google Test](https://github.com/google/googletest)
- [Boost.Test](https://www.boost.org/doc/libs/release/libs/test/)