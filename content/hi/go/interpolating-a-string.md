---
title:                "स्ट्रिंग का अंतर्कलन"
html_title:           "Arduino: स्ट्रिंग का अंतर्कलन"
simple_title:         "स्ट्रिंग का अंतर्कलन"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

# गो प्रोग्रामिंग: ➡ शब्दमाला इंटरपोलेशन क्यों व कैसे करें?

## क्या और क्यों?

*स्ट्रिंग इंटरपोलेशन* (String Interpolation) को हिंदी में "शब्दमाला अन्तर्कथन" कहा जाता है। यह किसी स्ट्रिंग में वेरिएबल्स को डायरेक्टली इन्कोर्पोरेट करने की प्रक्रिया है। प्रोग्रामर्स इसे कोड को और आसान, पठनीय और फ्लेक्सिबल बनाने के लिए करते हैं।

## कैसे करें:

Go की वर्तमान वर्जन में फ़ार्मेट पैकेज का उपयोग करके स्ट्रिंग इंटरपोलेशन कर सकते हैं:

```Go
package main
import "fmt"
func main() {
    name := "Golang"
    fmt.Printf("Hello, %s!", name)
}
```

आउटपुट:

```Go
Hello, Golang!
```
## गहरा डाइव:

1. *ऐतिहासिक संदर्भ*: C और C++ में printf फ़ंक्शन के जरिए स्ट्रिंग इंटरपोलेशन की प्रक्रिया हमेशा से होती थी। Go भी इसका पालन करता है, फार्मेट पैकेज के माध्यम से।

2. *विकल्प*: स्प्रिंट, Sprintf, Sprintln फ़ंक्शन्स को भी आसानी से इंटरपोलेशन के लिए उपयोग किया जा सकता है। ये फ़ंक्शन्स काफ़ी स्थिति विशेष में बेहतर हो सकते हैं।

3. *आवेदन विवरण*: गो में स्ट्रिंग इंटरपोलेशन को धारण करने के लिए वेरिएबल्स को "%s" स्ट्रिंग इंटरपोलेटर के साथ जोड़ा जाता है। इसका उपयोग करना एक काम करने का बेहतर और सुंदर तरीका हो सकता है।

## अन्य जानकारी:

- Go नियोक्ता के उदाहरण के लिए यहां जाएँ: [Go by example: String formatting](https://gobyexample.com/string-formatting)

- Go प्रेसेंटेशन के उद्धरण के लिए अधिक जानने के लिए: [Effective Go: Formatting] (https://golang.org/doc/effective_go#formatting) 

- स्टैक ओवरफ्लो पर Go स्ट्रिंग इंटरपोलेशन के बारे में सवाल: [Stackoverflow: Go string interpolation](https://stackoverflow.com/questions/42561276/how-to-log-with-variable-in-string-in-golang).