---
title:                "Elm: टेक्स्ट फाइल पढ़ना"
simple_title:         "टेक्स्ट फाइल पढ़ना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

# क्यों

कभी-कभी हमें एक पाठ फाइल से डेटा पढ़ने की जरूरत होती है, जैसे डेटाबेस के बजाय हम कोरासेल (corpus) में से समान शब्दों का अन्वेषण (lookup) करना चाहते हैं। इस लेख में, हम आपको Elm प्रोग्रामिंग के माध्यम से टेक्स्ट फाइल से डेटा पढ़ने के तरीके बताएंगे। 

# कैसे

```Elm
import Text

-- फाइल नाम का पथ निर्दिष्ट करें
filename = "data.txt"

-- फाइल को खोलें
file = File.fromPath filename

-- खुले हुए फाइल से पाठ पढ़ें
Text.readFile file
```

आउटपुट:
```
यह टेक्स्ट फाइल से डेटा पढ़ने का उदाहरण है।
```

इस उदाहरण में, हम ने `Text` मॉड्यूल को इम्पोर्ट किया और उसकी `readFile` फ़ंक्शन का उपयोग करके फाइल से पाठ पढ़ा है। 

# गहराई में जाएं

टेक्स्ट फाइलों से डेटा पढ़ने के साथ-साथ आप उनमें से डेटा अद्यतन भी कर सकते हैं। आप इस लेख के अंत में दिए गए जानकारी स्रोत बनाकर या इसे निकालकर अपने परियोजना में इस्तेमाल कर सकते हैं। साथ ही, आप टेक्स्ट फाइलों से डेटा पढ़ने का अधिक उदाहरण भी देख सकते हैं।

# देखें भी

- [Elm अधिक विवरण](https://guide.elm-lang.org/get_started.html)
- [Elm अक्षरपट](https://elm-autograph.herokuapp.com/)
- [टेक्स्ट फाइल से डेटा पढ़ने का अधिक उदाहरण](https://package.elm-lang.org/packages/elm/file/latest/File#readFile)