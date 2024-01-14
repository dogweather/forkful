---
title:    "C#: टेस्ट लिखना"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Kyun:

Unit tests likhne ka sabse bada fayda yeh hai ki woh aapke code ko test karne mei madad karte hain, taki aapke code mei kisi bhi tarah ki errors ya bugs na ho. Tests likhne se aap apne code ki quality aur stability ko improve kar sakte hain.

## Kaise likhein:

Tests likhne ke liye aapko C# programming language ki knowledge ki zarurat hogi. Neeche diye gaye code blocks mei aapko ek basic test likhne ka tarika dikhaya gaya hai. Aap inn examples ko apne code mei incorporate karke apne code ko test kar sakte hain.

```C#
[Test]
public void TestAddition()
{
  int a = 5;
  int b = 10;
  int sum = a + b;

  Assert.AreEqual(15, sum);
}
```

Is code block mei humne ek test likha hai jisme humne 'a' aur 'b' variables ko add karke unki sahi value check ki hai. Yeh tarika bahut hi simple hai aur aap apne code ke liye alag alag tests likh sakte hain.

## Gehri Jaankari:

Tests likhne ke liye do pramukh frameworks hai - NUnit aur MSTest. Dono hi bahut hi powerful hai aur aapko apne code ko test karne mei madad karte hain. Tests likhne ke bhi kuch zaruri tips hain:

- Har function ya method ke liye ek test likhna chahiye.
- Negative scenarios bhi test karna na bhulein.
- Tests ko regularly update karein aur maintain karein.

## Dekhein bhi:

- [Unit Testing in C#](https://docs.microsoft.com/en-us/dotnet/core/testing/unit-testing-with-nunit)
- [Introduction to Test Driven Development (TDD)](https://www.freecodecamp.org/news/test-driven-development-what-it-is-and-what-it-is-not-41fa6bca02a2/)

Dostoin, tests likhna kisi bhi programming language mei bahut hi zaruri hai. Isse aap apne code ki quality aur performance ko improve kar sakte hain. Umeed hai ki yeh article aapke liye helpful hoga. Happy coding!