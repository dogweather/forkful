---
title:                "भविष्य या भूतकाल में एक तिथि को गणना करना"
html_title:           "C++: भविष्य या भूतकाल में एक तिथि को गणना करना"
simple_title:         "भविष्य या भूतकाल में एक तिथि को गणना करना"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Kyun
Kya aapne kabhi socha hai ki aapne kisi event ya appointment ko kitne din baad ya pehle se schedule kiya hai? Apne future plans ko organize karte waqt, hume ek particular date tak pahunchne ki zarurat hoti hai, isliye future aur past mein taareekh calculate karna important ho jata hai.

## Kaise Karein
```C++
#include<iostream>
using namespace std;
int main()
{
  int current_day = 8; // Current day of the month
  int current_month = 6; // Current month
  int current_year = 2021; // Current year

  // To calculate a date in the future
  int future_day = current_day + 10; // Add 10 days to the current day
  int future_month = current_month; // Future month will be same as current month
  int future_year = current_year; // Future year will be same as current year

  // Check if the future day exceeds number of days in the month
  if(future_day > 31) 
  {
    // Add remaining days to the next month
    future_day = future_day - 31;
    future_month += 1;
  }

  // Check if the future month exceeds 12
  if(future_month > 12)
  {
    // Reset month to 1 and increment year
    future_month = 1;
    future_year += 1;
  }

  // Output the future date
  cout<<"Future date: "<<future_day<<"/"<<future_month<<"/"<<future_year<<endl;

  // To calculate a date in the past
  int past_day = current_day - 5; // Subtract 5 days from the current day
  int past_month = current_month; // Past month will be same as current month
  int past_year = current_year; // Past year will be same as current year

  // Check if past day is less than 1
  if(past_day < 1) 
  {
    // Calculate remaining days to be subtracted from previous month
    past_day = 31 - (5 - current_day);
    past_month -= 1;
  }

  // Check if past month is less than 1
  if(past_month < 1)
  {
    // Reset month to 12 and decrement year
    past_month = 12;
    past_year -= 1;
  }

  // Output the past date
  cout<<"Past date: "<<past_day<<"/"<<past_month<<"/"<<past_year<<endl;

  return 0; 
}
```

## Deep Dive
Is code mein, hum current taareekh ko lekar future aur past dates ko calculate karne ke liye simple logic ka upyog kiya hai. Hum baad mein days, months aur years ko adjust karte hain jaise ki automation system bhi karta hai. Is tarah, hum ek accurate date ko calculate kar sakte hain.

## Dekhna Bhi Pade
Agar aapko coding language aur logic ke baare mein aur jaankari chahiye toh neeche diye gaye links ko zarur check karein:
- [C++ Programming Tutorial](https://www.tutorialspoint.com/cplusplus/index.htm)
- [Logic Building in Programming](https://www.geeksforgeeks.org/top-5-must-know-algorithms-and-data-structures-for-competitive-programming/)