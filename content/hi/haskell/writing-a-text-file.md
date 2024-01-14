---
title:                "Haskell: टेक्स्ट फ़ाइल लिखना"
simple_title:         "टेक्स्ट फ़ाइल लिखना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों

एक पाठ फ़ाइल लिखने में क्यों लोग रुचि रखते हैं? विस्तार से जानने के लिए आश्चर्यजनक कारण है।

## कैसे करें

```Haskell
main = do
    let text = "हैस्केल कोडिंग के साथ एक पाठ फ़ाइल लिखना"
    writeFile "example.txt" text
```
इस कोड ब्लॉक का उत्पादन काफी सादा होगा - एक new लाइन से संगत डेमोनोईडिंग।

## डीप डाइव

पाठ फ़ाइल लिखने का और गहरा ज्ञान हासिल करने के लिए, आप एक फाइल हैंडल का उपयोग कर सकते हैं। यह आपको फ़ाइल में फोरमैटिंग, संपादन और अन्य उपयोगी फ़ंक्शंस का उपयोग करने की अनुमति देता है।

## देखें भी

- [Haskell में टेक्स्ट फ़ाइल लिखना] (https://www.geeksforgeeks.org/writing-text-file-using-haskell/)
- [Haskell के साथ पाठ फाइल पढ़ना और लिखना] (https://www.tutorialspoint.com/haskell/haskell_input_output.htm)
- [Haskell फ़ाइल संबंध] (https://www.haskell.org/haskellwiki/Introduction_to_Haskell_IO/Files_and_handles)