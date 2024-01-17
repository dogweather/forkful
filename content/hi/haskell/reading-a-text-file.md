---
title:                "एक लेख फाइल को पढ़ना"
html_title:           "Haskell: एक लेख फाइल को पढ़ना"
simple_title:         "एक लेख फाइल को पढ़ना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
टेक्स्ट फाइल पढ़ना है क्या? यह सवाल बहुत से Haskell प्रोग्रामरों के मन में आता है। टेक्स्ट फाइल पढ़ने से, हम अपनी कोड को एक अलग फाइल में संग्रहित कर सकते हैं और उसको सम्पादित भी कर सकते हैं। इससे हमारे कोड को आसानी से प्रबंधित किया जा सकता है।

## कैसे करें?
Haskell में टेक्स्ट फाइल पढ़ने के लिए आमतौर पर इस्तेमाल की जाने वाली ```readFile``` फ़ंक्शन है। इसकी मदद से हम कोई भी टेक्स्ट फाइल पढ़ सकते हैं और काम आने वाली डेटा को उपयोग कर सकते हैं। नीचे एक उदाहरण दिया गया है:

```Haskell
main = do
  content <- readFile "data.txt"
  putStrLn content
```

उपरोक्त कोड से, हम फ़ाइल ```data.txt``` को पढ़ सकते हैं और उसकी सामग्री को हमारी एक्सपेक्टेड फाइल से प्रिंट कर सकते हैं।

## गहराई तक जाएं
Haskell में टेक्स्ट फाइल पढ़ने की इस तकनीक को विकास का कारण माना जाता है। इस तकनीक को बनाने के लिए अन्य प्रोग्रामिंग भाषाओं से प्रेरणा ली गई है, जैसे AWK, Perl, और Python। इसके अलावा, Haskell में अन्य तरीके भी हैं जिनसे आप टेक्स्ट फाइल पढ़ सकते हैं, जैसे ```System.IO``` में सुझाए गए फ़ंक्शन्स। पढ़ने के साथ-साथ, आप अपनी कोड को और भी प्रबंध्य और बनावटशील बनाने के लिए भी टेक्स्ट फाइल पढ़ने का उपयोग कर सकते हैं।

## और हमें देखें
- [Haskell Wiki on the ```readFile``` function](https://wiki.haskell.org/Reading_text_from_a_file_with_readFile)
- [Official Haskell documentation on the ```System.IO``` module](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base/System-IO.html)