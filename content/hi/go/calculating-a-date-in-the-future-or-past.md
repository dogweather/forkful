---
title:                "भविष्य या अतीत में तारीख की गणना"
html_title:           "Go: भविष्य या अतीत में तारीख की गणना"
simple_title:         "भविष्य या अतीत में तारीख की गणना"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

भविष्य या अतीत में किसी तारीख की गणना करना से तात्पर्य है किसी मानसिक संख्या, जैसे कि दिनों, महीनों, या वर्षों को वर्तमान तारीख से जोड़ना या घटाना। प्रोग्रामर्स करते हैं इसे तब जब उन्हें यह समझने की आवश्यकता होती है कि किसी दिन की तारीख क्या होगी, जैसे कि उत्पाद लॉन्च की अगली तारीख। 

## कैसे करें:

Go भाषा में, हम `Add` और `Sub` फ़ंक्शन का उपयोग करके `time.Time` ऑब्जेक्ट में समय की गणना कर सकते हैं।

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	t := time.Now()
	
	// दिनों की संख्या का जोड़ना
	future := t.Add(24 * time.Hour)
	fmt.Println("भविष्य की तारीख: ", future)

	// दिनों की संख्या को घटाना
	past := t.Add(-24 * time.Hour)
	fmt.Println("पिछली तारीख: ", past)
}
```

## गहरी जानकारी

(1) ऐतिहासिक सन्दर्भ: Go भाषा 2007 में गूगल द्वारा विकसित की गई थी और इसने तत्कालिन प्रोग्रामिंग भाषाओं की कमियों को दूर किया। `time` पैकेज एक उदाहरण है इसका, जिसने समय और तारीख को हैंडल करने का एक सरल और ढंग से व्यवस्थित ढंग प्रदान किया।

(2) विकल्प: आप Java, Python, Ruby आदि भाषाओं का उपयोग भी कर सकते हैं फ़्यूचर या पास्ट तारीख की गणना के लिए, पर Go में यह अधिक सुगम और स्पष्ट होता है।

(3) कैसे काम करता है: `Add` और `Sub` फ़ंक्शन Go के `time` पैकेज में सख्ती से बांधे गए होते हैं। वे दिन, घंटे, मिनट, या सेकंड के आधार पर समय इकाइयों को सही तरीके से हैंडल करते हैं। 

## और देखें:

1. [Go by Example: Time](https://gobyexample.com/time)
2. [Go Documentation: time package](https://golang.org/pkg/time/)
3. [A Tour of Go: Time](https://tour.golang.org/welcome/4)