---
title:                "भविष्य या अतीत में तिथि की गणना"
html_title:           "Go: भविष्य या अतीत में तिथि की गणना"
simple_title:         "भविष्य या अतीत में तिथि की गणना"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Kyu
*Agar aap apne program mein kisi bhi tarikh (future ya past) ka calculation karna chahte hain, to aapko is technique ki jaroorat padegi. Ye aapke program ko dynamic aur user-friendly banane mein madad karegi.*

## Kaise
Agar aapko kisi bhi future ya past tarikh ka calculation karna hai, to aapko isme kuch simple steps follow karne honge:

1. Pehle se define kariye - Kis tarikh se aapka calculation start karna hai.
2. Phir ```time``` aur ```duration``` packages ko import karein.
3. Ab ```AddDate``` method ka use karein, jiske saath aap apni tarikh ko add kar sakte hain.
4. Aap apne code mein ```fmt.Printf``` ka use karke desired output print kar sakte hain.

Ab dekhate hain kis tarah se aap is technique ka use kar sakte hain. Neeche diye gaye code blocks mein aapko coding examples aur sample output milenge:

**Calculating a future date:**
```
package main

import (
	"fmt"
	"time"
)

func main() {
	start := time.Date(2020, time.April, 5, 8, 30, 0, 0, time.UTC)
	futureDate := start.AddDate(0, 0, 10) //adding 10 days to start date
	fmt.Printf("The future date is: %v", futureDate)
}
```

Output:
```
The future date is: 2020-04-15 08:30:00 +0000 UTC
```

**Calculating a past date:**
```
package main

import (
	"fmt"
	"time"
)

func main() {
	start := time.Date(2020, time.April, 15, 8, 30, 0, 0, time.UTC)
	pastDate := start.AddDate(0, 0, -10) //subtracting 10 days from start date
	fmt.Printf("The past date is: %v", pastDate)
}
```

Output:
```
The past date is: 2020-04-05 08:30:00 +0000 UTC
```

## Deep Dive
Calculating dates in the future or past is a common task in programming, and Go provides us with the powerful ```time``` package to make this task easier. The ```AddDate``` method takes in three arguments - years, months and days - and adds them to the start date to calculate the desired date. We can also manipulate the dates by adding or subtracting different values for these arguments.

## Dekhiye bhi
1. [Official Go Language website](https://golang.org/)
2. [Tutorialspoint - Go Programming Language](https://www.tutorialspoint.com/go/index.htm)
3. [Code Academy - Learn Go](https://www.codecademy.com/learn/learn-go)