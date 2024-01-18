---
title:                "एक स्ट्रिंग से तिथि विश्लेषण"
html_title:           "Go: एक स्ट्रिंग से तिथि विश्लेषण"
simple_title:         "एक स्ट्रिंग से तिथि विश्लेषण"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

पार्सिंग एक तारीख को स्ट्रिंग से हटाना होता है, जिससे हम उस तारीख को प्रोग्राम में उपयोग कर सकते हैं। प्रोग्रामर्स इस तकनीक का उपयोग करने से पूर्व, तारीख को फार्मेट और स्ट्रिंग में लिखने के लिए भिन्न फॉर्मैट्स या स्टाइल में अपनी तरकीब निर्धारित कर सकते हैं।

## कैसे करें:

 ```Go 
package main 
import ( 
	"fmt" 
	"time" 
) 
func main() { 
	// डेट स्ट्रिंग को तारीख में परिवर्तित करने के लिए Parse का उपयोग करें
	dateString := "01/02/2006" // स्वाभाविक फॉर्मेट 
	date, _ := time.Parse("01/02/2006", dateString) 
	fmt.Println(date) // स्ट्रिंग से तारीख को प्रिंट करें 
	// आउटपुट: 2006-01-02 00:00:00 +0000 UTC

	// विभिन्न डेट स्ट्रिंग के लिए अलग फॉर्मेट का उपयोग करें 
	dateString2 := "01/02/06" // अन्य फॉर्मेट 
	date2, _ := time.Parse("01/02/06", dateString2) 
	fmt.Println(date2) // स्ट्रिंग से तारीख को प्रिंट करें 
	// आउटपुट: 2006-01-02 00:00:00 +0000 UTC
} 
```

## गहराई में जानें:

पिछले साल, पार्सिंग डेट स्ट्रिंग के लिए अनेक तरीकों का आना शुरू हुआ है जिसमें एक्सटर्नल लाइब्रेरी का भी शामिल है। लोकप्रिय विकल्पों में मौजूद हैं time.Parse(), time.ParseInLocation(), और time.ParseDuration()। ये विभिन्न रुचि के साथ काम करते हैं और किसी भी स्ट्रिंग से तारीख परिवर्तित करने के लिए उपयुक्त हैं। इन कार्यों के अलावा, गो में बहुत से साधारण तरीके हैं जिनका उपयोग किया जा सकता है।

## अधिक देखें:

- [Go Language Specification - The Go Programming Language](https://golang.org/ref/spec)
- [dateparse - GoDoc](https://godoc.org/github.com/araddon/dateparse)
- [Go Time Format Cheat Sheet - Kyo](https://dev.to/kyoh86/go-time-format-cheat-sheet-3a24)