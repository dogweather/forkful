---
title:    "Elm: डाइरेक्ट्री की उपस्थिति की जाँच करना"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्यों
एक डायरेक्टरी मौजूद है या नहीं यह जाँचने में व्यक्ति क्यों लगने को उत्सुक होगा कुछ उत्तर देती हैं।

## कैसे करें
डायरेक्टरी का अस्तित्व जांचने के लिए विभिन्न प्रोग्रामिंग भाषाओं में से Elm में किस तरहतो सेतुप किया जा सकता है।

```Elm
import File

doesDirectoryExist : String -> Cmd msg
doesDirectoryExist path =
    Task.perform DoesDirectoryExist (File.exists path)

type Msg
    = DoesDirectoryExist Bool
```

आउटपुट:
```Elm
DoesDirectoryExist False
```

## गहराई से जांचें
डायरेक्टरी का अस्तित्व जांचने के लिए हमें पहले से ही जानना चाहिए कि एक डायरेक्टरी की विशेषताएं क्या हैं और कैसे हम इसे उपयोग कर सकते हैं। Elm में हम डायरेक्टरी को अस्तित्व जांचने के लिए File लाइब्रेरी का उपयोग कर सकते हैं। हम उसी कोड के माध्यम से कैसे हमें डायरेक्टरी के अस्तित्व का पता लगा सकते हैं, इस बारे में गहराई से जानते हैं।

## देखें
- [Elm लाइब्रेरी मानव अभिव्यक्ति के लिए एक फंक्शनल प्रोग्रामिंग भाषा](https://guide.elm-lang.org/)
- [Elm में हम परिवर्तनात्मक और स्थाई डाटा को कैसे प्रबंधित कर सकते हैं](https://www.elm-tutorial.org/en/)
- [Elm Community हैंडबुक](https://elm-community.github.io/elm-faq/)