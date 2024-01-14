---
title:                "Go: csv के साथ काम करना"
simple_title:         "csv के साथ काम करना"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/working-with-csv.md"
---

{{< edit_this_page >}}

# क्यों

CSV एक उपयोगी और लोकप्रिय फाइल प्रारूप है जो डेटा को संग्रहीत करने और इस्तेमाल करने को आसान बनाता है। इसके साथ जुड़े हुए काम करना बहुत सरल और मजेदार हो सकता है। इस ब्लॉग पोस्ट में, हम आपको CSV के साथ काम करने के लिए गो प्रोग्रामिंग का उपयोग करने के लिए कुछ नीव और टिप्स देंगे।

## कैसे करें

CSV फ़ाइलों को अपने गो प्रोग्राम में लोड करने के लिए, हमें पहले CSV पैकेज को इम्पोर्ट करना होगा। यहां हम CSV फ़ाइल को लोड करने और उसे पार्स करने का एक सरल उदाहरण देखेंगे:

```Go
package main

import (
	"encoding/csv"
	"fmt"
	"os"
)

func main() {
	// CSV फ़ाइल खोलें
	file, err := os.Open("example.csv")
	if err != nil {
		fmt.Println("फ़ाइल खोलने में त्रुटि:", err)
		return
	}
	defer file.Close()

	// CSV फ़ाइल को पार्स करें
	reader := csv.NewReader(file)
	for {
		record, err := reader.Read()
		if err != nil {
			fmt.Println("पार्स करने में त्रुटि:", err)
			return
		}
		// प्रत्येक पंक्ति के लिए डेटा छापें
		fmt.Println(record)
	}
}
```

इस कोड के आउटपुट को देखने के लिए आपको अपने मशीन पर कुछ सामान्य डेटा होना चाहिए जो आप एक CSV फ़ाइल में संग्रहीत करना चाहते हैं। यदि आपको उपलब्ध डेटा नहीं है, तो आप हमारे एक्साम्पल CSV फ़ाइल का उपयोग कर सकते हैं।

## गहराई में जाएं

CSV फाइलों के साथ काम करते समय कुछ गहराई में जाने से आप अधिक उपयोगी तरीकों को जान सकते हैं और अपने गो प्रोग्राम को और अच्छा बना सकते हैं