---
title:                "एरर्स को हैंडल करना"
date:                  2024-01-26T00:57:21.388571-07:00
model:                 gpt-4-1106-preview
simple_title:         "एरर्स को हैंडल करना"
programming_language: "Go"
category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/handling-errors.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

Go में त्रुटि संभालना का तात्पर्य रनटाइम की अप्रत्याशित समस्याओं को सुचारू रूप से पकड़ने और प्रतिक्रिया देने से है। हम इसे इसलिए करते हैं ताकि क्रैशेस को रोका जा सके और यह सुनिश्चित किया जा सके कि हमारे प्रोग्राम तब भी अनुमानित रूप से काम करें, जब चीजें गलत हो जाएँ।

## कैसे करें:

Go में स्पष्ट त्रुटि संभालना का प्रयोग किया जाता है। इसका मतलब है कि आप हर बार जब भी कोई फंक्शन कॉल करेंगे तो आपको चेक करना होगा कि क्या उसने कोई त्रुटि लौटाई है। कोई अपवाद नहीं। देखिए यह कैसा दिखता है:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	err := doSomething()
	if err != nil {
		fmt.Println("उह ओह:", err)
		os.Exit(1)
	}
}

func doSomething() error {
	// मान लेते हैं कुछ गलत हो गया
	return fmt.Errorf("कुछ गलत हो गया")
}
```

इसे चलाएं, और आपको मिलेगा:

```
उह ओह: कुछ गलत हो गया
```

लेकिन अगर यह सफल होता है तो?

```Go
func doSomething() error {
	// इस बार सब ठीक है
	return nil
}
```

कोई आउटपुट नहीं। शानदार, कोई खबर नहीं अच्छी खबर है।

## गहराई में:

Go में, त्रुटि संभालना के संबंध में कई मतभेद रहे हैं। शुरू से ही, Go ने अपवादों (exceptions) को छोड़ कर और अधिक स्पष्ट दृष्टिकोण अपनाने का निर्णय लिया, जिसे कुछ डेवलपर्स सरलता के लिए पसंद करते हैं और अन्य इसे वर्णनात्मक पाते हैं। बिल्ट-इन `error` प्रकार एक इंटरफेस है। किसी भी प्रकार की विधि `Error() string` इसे संतुष्ट करती है। यह Go की सादगी और स्पष्टता की आदर्शों के साथ जुड़ा हुआ है।

विकल्प? `panic` और `recover` की जोड़ी है, लेकिन वे असाधारण मामलों के लिए हैं (शब्दों का खेल माना जाता है) जब कार्यक्रम जारी नहीं रख सकता। `panic` को समझें एजेक्ट बटन के रूप में जिसे आप तब दबाते हैं जब आप जानते हैं कि वापसी का कोई रास्ता नहीं है। इसका उपयोग विरले में करें।

मुख्यधारा की त्रुटि संभालने के लिए, Go 1.13 ने त्रुटि आवरण (error wrapping) को पेश किया, जिससे `errors.Is()` और `errors.As()` जैसे फंक्शन का उपयोग करके "त्रुटि श्रृंखला" को समझना आसान हो गया।

## देखें भी:

Go में त्रुटि संभालना से संबंधित सभी चीजें के लिए:

- गो ब्लॉग पर त्रुटि संभालना: [https://blog.golang.org/error-handling-and-go](https://blog.golang.org/error-handling-and-go)
- प्रभावी Go – त्रुटि संभालना अनुभाग: [https://golang.org/doc/effective_go#errors](https://golang.org/doc/effective_go#errors)
- Go 1.13 त्रुटि आवरण प्रलेखन: [https://golang.org/doc/go1.13#error_wrapping](https://golang.org/doc/go1.13#error_wrapping)
- डेव चेनी की पोस्ट त्रुटि संभालना रणनीतियों पर: [https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully](https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully)