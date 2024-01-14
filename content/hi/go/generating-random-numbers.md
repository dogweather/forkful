---
title:                "Go: रैंडम नंबर्स उत्पन्न करना"
programming_language: "Go"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्यों

अक्सर हमारे कोड में हमें यादें बनाने के लिए या टेस्टिंग के लिए रैंडम नंबर की आवश्यकता होती है। यह आर्टिकल तुम्हें Go में रैंडम नंबर जेनरेट करने के लिए उपयोगी तकनीकें सिखाएगा।

## कैसे

```Go
func main() {
    // सेटिंग रैंडम सीड (Seed)
    rand.Seed(time.Now().UnixNano())

    // 1 से 100 के बीच रैंडम नंबर जेनरेट करना
    fmt.Println("1 से 100 के बीच कोई रैंडम संख्या:", rand.Intn(100))

    // 10 से 50 के बीच रैंडम नंबर जेनरेट करना
    fmt.Println("10 से 50 के बीच कोई अन्य रैंडम संख्या:", rand.Intn(40) + 10)
}
```

आउटपुट:
```
1 से 100 के बीच कोई रैंडम संख्या: 64
10 से 50 के बीच कोई अन्य रैंडम संख्या: 34
```

## गहराई में

Go में रैंडम नंबर जेनरेट करने के लिए, हम `math/rand` और `time` पैकेज का उपयोग करते हैं। `Seed()` फ़ंक्शन से हम सीड सेट कर सकते हैं जो रैंडम नंबर जेनरेट करते समय उपयोगी होता है। `Intn()` फ़ंक्शन से हम दिए गए रेंज के बीच रैंडम नंबर जेनरेट कर सकते हैं।

## इससे संबंधित

- [Go डॉक्यूमेंटेशन - `math/rand` पैकेज](https://golang.org/pkg/math/rand/)
- [Go डॉक्यूमेंटेशन - `time` पैकेज](https://golang.org/pkg/time/)
- [Go रैंडम नंबर जेनरेट करने के 5 तरीके](https://www.callicoder.com/golang-random-number-generator/)
- [Go में रैंडम नंबर जेनरेट करने का वीडियो ट्यूटोरियल](https://www.youtube.com/watch?v=SnnjE48cA4k)