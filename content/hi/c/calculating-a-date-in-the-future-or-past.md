---
title:    "C: भविष्य या भूतकाल में एक तारीख का गणना करना"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Kyu
Koi bhi insaan aaj kal apne jeevan mein samay ka pata rakhna chahta hai. Isiliye, date ko bhavishya ya bhoot avadhi mein ganana karna bhi bahut mahatvapurn dikhai deta hai. Kisi ke janm din ka pata lagana ho ya fir kisi upcoming festival ko celebrate karne ke liye date calculate karna ho, ye sab kaam mujhe aur mere jaise developers ke liye bahut aasan ho jata hai agar hum C programming ke madhyam se jaane.

## Kaise
Sabse pehle, hum header files ko include karna hoga jaise ki "stdio.h" aur "conio.h". Uske baad, hum ek user-defined function likhenge jise hum "calculateDate" naam denge. Is function mein, hum variables declare karenge jaise ki "day", "month", aur "year" aur unhe input lene ke liye hum "scanf" function ka use karenge. Fir hum date ko calculate karne ke liye "if-else" statements ka upyog karenge. Code block ke liye, ye algorithm follow kiya ja sakta hai:

```
#include<stdio.h>
#include<conio.h>
void calculateDate()
{
	int day, month, year;
	printf("Enter the day: ");
	scanf("%d", &day);
	printf("Enter the month: ");
	scanf("%d", &month);
	printf("Enter the year: ");
	scanf("%d", &year);
	if(day == 31 && month == 12)
	{
		day = 1;
		month = 1;
		year++;
	}
	else if((day == 28 || day == 29) && month == 2)
	{
		day = 1;
		month++;
	}
	else if((day == 30) && (month == 4 || month == 6 || month == 9 || month == 11))
	{
		day = 1;
		month++;
	}
	else
	{
		day++;
	}
	printf("The calculated date is: %d-%d-%d", day, month, year);
}
int main()
{
	calculateDate();
	return 0;
}
```

Output ke liye, agar hum 29th February 2020 ko calculate karenge, toh hume 1st March 2020 milega.

## Gehri Talash
Date ko future ya past mein calculate karna kaafi asaan hai, lekin iske peeche ka logic sahi se samajhna bahut zaroori hai. Humne "if-else" statements ka use isliye kiya kyunki humne saare scenarios ko consider kiya hai jaise ki leap year aur number of days in a month. Agar hum ye logic samajh lete hai, toh hum apne code mein modifications kar sakte hai aur kisi bhi date ko calculate kar sakte hai.

## Dekhein bhi
Agar aapko C programming se related aur kaafi interesting topics jaise ki arrays, functions, loops, etc. ke baare mein jaanna hai, toh neeche diye gaye links aapke kaam aa sakte hai:

- [Arrays tutorial](https://www.programiz.com/c-programming/c-arrays)
- [Functions tutorial](https://www.programiz.com/c-programming/c-functions)
- [Loops tutorial](https://www.programiz.com/c-programming/c-loops)