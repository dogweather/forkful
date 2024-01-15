---
title:                "एक पैटर्न को मेल खाते करके अक्षरों को हटाना।"
html_title:           "Go: एक पैटर्न को मेल खाते करके अक्षरों को हटाना।"
simple_title:         "एक पैटर्न को मेल खाते करके अक्षरों को हटाना।"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## इसका मकसद क्या है?

अगर आप गो (Go) प्रोग्रामिंग में नए हैं, तो आपने शायद पैटर्न मैचिंग काम करते हुए कुछ तरीके देखे होंगे। इसमें आपको दिए गए पैटर्न में से एक पैटर्न को हटाने की आवश्यकता हो सकती है। हम यहां आपको बताएंगे कि अपनी गो प्रोग्राम्स में आप कैसे चुने गए पैटर्न को हटा सकते हैं।

## कैसे करें?

आपको अपने कोड में `strings` पैकेज को इम्पोर्ट करना होगा। इसके बाद, आप `strings.ReplaceAll()` फंक्शन का इस्तेमाल करके स्ट्रिंग को उस पैटर्न से खाली कर सकते हैं। नीचे कोड उदाहरण देखें:

```Go
import "strings"

func main() {
  originalString := "Hello Gophers! This article is for Go beginners."
  newString := strings.ReplaceAll(originalString, "Go", "")
  fmt.Println(newString)

  // Output:
  // Hello phers! This article is for  beginners.
}
```

## डीप डाइव

`ReplaceAll()` फंक्शन स्ट्रिंग को खाली करने के अलावा और भी कुछ कर सकता है। आप इस फंक्शन के इस्तेमाल से एक से अधिक पैटर्न को एक साथ हटा सकते हैं या फिर उनमें से कुछ को रीप्लेस कर सकते हैं। आप `strings.Replace()` फंक्शन का भी इस्तेमाल कर सकते हैं जो पैटर्न को `n` बार रिपीट करेगा।

## देखें भी

- Go Documentation on `strings` package: https://golang.org/pkg/strings/
- Tutorial on Go string manipulation: https://www.freecodecamp.org/news/strings-in-go-tutorial/