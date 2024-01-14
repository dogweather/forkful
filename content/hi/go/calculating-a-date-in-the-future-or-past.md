---
title:                "Go: भविष्य या भूतकाल में तारीख का गणना"
simple_title:         "भविष्य या भूतकाल में तारीख का गणना"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्यों

आगे या पीछे की तारीख का हिसाब लगाने में क्यों गो भाषा को चुनाव किया जाए?

## कैसे करें

आपके पास कितना भी अनुभव हो, गो भाषा में आसानी से तारीख का हिसाब लगाना संभव है। आप नीचे दिए गए कोड ब्लॉक के माध्यम से इसे कैसे करें देख सकते हैं:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// आज की तारीख लेना
	today := time.Now()

	// आगे का आधे साल का हिसाब लगाना
	future := today.AddDate(0, 6, 0)
	fmt.Println("आगे का आधे साल:", future.Format("02 January 2006"))

	// पीछे का एक साल का हिसाब लगाना
	past := today.AddDate(-1, 0, 0)
	fmt.Println("पीछे का एक साल:", past.Format("02 January 2006"))

	// आगे का तीन महीने का हिसाब लगाना
	futureMonth := today.AddDate(0, 3, 0)
	fmt.Println("आगे का तीन महीने:", futureMonth.Format("02 January 2006"))

	// पीछे का दस दिन का हिसाब लगाना
	pastDays := today.AddDate(0, 0, -10)
	fmt.Println("पीछे का दस दिन:", pastDays.Format("02 January 2006"))
}
```

आउटपुट:

```
आगे का आधे साल: 21 मार्च 2021
पीछे का एक साल: 21 सितंबर 2019
आगे का तीन महीने: 21 जनवरी 2020
पीछे का दस दिन: 11 सितंबर 2019
```

## गहराई में जाएं

तारीख का हिसाब लगाने में गो भाषा एक बहुत ही शक्तिशाली उपकरण है। यह आपको अनेक तरीकों से तारीख का हिसाब लगाने की सुविधा प्रदान करता है। आप `AddDate()` फंक्शन के जरिए आगे या पीछे की तारीख को हिसाब लगा सकते हैं जो आपको साल, महीने और दिनों के साथ आधे साल, एक साल या कुछ दिनों का हिसाब लगाने में मदद करता