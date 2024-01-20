---
title:                "पाठ की खोज और प्रतिस्थापन"
html_title:           "Bash: पाठ की खोज और प्रतिस्थापन"
simple_title:         "पाठ की खोज और प्रतिस्थापन"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

टेक्स्ट खोजना और बदलना एक क्रिया है, जिसमें हम एक स्पष्ट टेक्स्ट खोजते हैं और उसे किसी अन्य टेक्स्ट से बदल देते हैं। यह डेटा संसाधन और प्रसंस्करण के प्रक्रियाओं को आसान बनाने और प्रोग्राम की दक्षता को बढ़ाने के लिए कार्यकर्ताओं द्वारा काम में लाया जाता है। 

## ऐसे करें:

```Go
package main
import "strings"
import "fmt"

func main() {
    str := "Welcome to Go Programming!"
    newStr := strings.Replace(str, "Go", "Hindi", -1)
    fmt.Println(newStr)
}
```

साउटपुट:

```Go
Welcome to Hindi Programming!
```

## गहरा डाइव:

औद्योगिक आरंभ में, यह टेक्स्ट खोजने और बदलने की क्रिया मैन्युअल रूप से काम में ली गई थी जिसमें ज्यादा समय और प्रयास लगता था। लेकिन Go और अन्य प्रोग्रामिंग भाषाओं के निर्माण से, इसे संगणकीयता से और कुशलतापूर्वक किया जा सकता है। संबंधित `strings.Replace()` function का उपयोग करके Go में टेक्स्ट को खोजा और बदला जा सकता है। जैसा कि ऐसे तरीकों में संभवतः विचलन शामिल हो सकते हैं, कई अन्य समाधान, जैसे कि regex, पहुंचने में समय लेने वाले और जटिल खोजों के लिए उपयोगी हैं। 

## भी देखें:

- Go द्वारा और अधिक Text Manipulations: https://golang.org/pkg/strings/
- Regex के साथ खोजें और बदलें: https://golang.org/pkg/regexp/