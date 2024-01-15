---
title:                "दो तारीखों की तुलना करना"
html_title:           "C++: दो तारीखों की तुलना करना"
simple_title:         "दो तारीखों की तुलना करना"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Kyu: Kisi ko do tarikhon ko tulna karne me kyuki ye ek aam kam hai aur hume pata lagana hai ki kon si tarikh badi ya choti hai.

## Kaise Kare:
### Tarikhon ko Date Objects me Convert Kare:
```C++
date date1(2020, 10, 15); // date1 ko 15 October, 2020 me set kare
date date2(2021, 3, 8); // date2 ko 8 March, 2021 me set kare
```

### Tarikhon ko Tulna Kare:
```C++
if (date1 == date2) { 
    cout << "Dono tarikh barabar hai." << endl;
} else if (date1 > date2) {
    cout << "Date1 jyada badi hai." << endl;
} else {
    cout << "Date2 jyada badi hai." << endl;
}
```

### Sample Output:
```
Dono tarikh barabar hai.
```

## Gehri Jankari:
Tarikhon ko tulna karne ke liye, hume use compare operators (==, >, <) ka use karna hota hai. Date Objects ko create karne ke liye, hume "iomanip" header file ko include karna hota hai.

## Dekhiye Bhi:
[Hindi: Comparison Operators](https://www.programiz.com/cpp-programming/comparison-operators-hindi)

[Hindi: Date Manipulation](https://mksystems.co.in/cpp-date-and-time-functions-in-hindi/)