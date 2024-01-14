---
title:    "C++: टेस्ट लिखना"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Kyun

Teston ko likhne mein aapko kyun samay dena chahiye? Vyakti apane code ki quality aur functionality ko guarantee karne ke liye, code mein ki jane wali changes ka asar samajhne ke liye test likhte hain.

## Kaise Karein

Aap C++ programming language se familiar hain aur apne code mein changes karne ke liye uttejit hain. Lekin pareshan mat honein! Aapke paas tests likhne ke liye sampoorna control hai.

```C++
// Ye ek simple test hai jo check karta hai ki "sum" function sahi se kaam kar raha hai ya nahi.
#include<iostream>
#include<assert.h>

// Sum function ko declare aur define karein.
int sum(int a, int b) {
  return a + b;
}

int main() {
  assert(sum(2, 3) == 5);
  return 0;
}
```

Agar aapne code ko compile kiya aur usme koi error nahi aaya, toh aapke test bhi sahi se chal rahe hain!

## Gahrayi Mein Jaayein

Aap tests ko sirf apne code ki quality aur functionality ko guarantee karne ke liye nahi likhte hain, balki ye aapko code ke gahrayi mein bhi le jate hain. Jab aap tests likhte hain, aapko code ke har hisse ko samajhne aur usme ki jane wali changes ka asar samajhne ka mauka milta hai. Isse aapko apne code ko improve karne aur bugs ko fix karne ka pata chalta hai.

## Dekhein Bhi

Agar aapko test likhne ka yeh tarika samajhna hai toh aap ye links dekh sakte hain:
- [C++ Unit Testing Tutorial](https://www.tutorialspoint.com/cpp/cpp_unit_testing.htm)
- [C++ Testing Frameworks](https://www.oreilly.com/library/view/using-cpp-unit/9780735689524/)

Bhi ki tarah aap apne coding skills aur knowledge ko improve karne ke liye [C++ tutorials](https://www.geeksforgeeks.org/c-plus-plus/) aur [practice problems](https://www.hackerrank.com/domains/cpp) dekh sakte hain.