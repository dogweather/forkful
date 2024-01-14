---
title:                "Fish Shell: पैटर्न से मेल खाते अक्षरों को हटाना"
simple_title:         "पैटर्न से मेल खाते अक्षरों को हटाना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्यों

अपने स्क्रिप्ट में सलाह चाहिए होने के कारण, आपको सक्रियता से समझना होगा कि पैटर्न से मेल खाने वाले अक्षर को हटाना क्यों जरूरी है।

## कैसे करें

अब, हम फिश शेल में चल रहे हैं क्मांड लाइन एप्लिकेशन है, हम पैटर्न से मेल खाने वाले अक्षर को हटाने के लिए इसका उपयोग कैसे कर सकते हैं। नीचे उदाहरण में, हम एक सरल स्ट्रिंग "Hello World!" को देखेंगे जिसमें से हम अक्षर 'o' को हटाना चाहते हैं।

```Fish Shell
set input "Hello World!"
echo $input | sed 's/o//g'
```

आउटपुट:

```Fish Shell
Hell Wrld!
```

## गहराई में जाएं

जब आप पर अधिक स्वाधीनता चाहते हैं और अपने स्क्रिप्ट में इस प्रकार के ऑपरेशन को अन्य कमांडों के साथ कंबाइन करना चाहते हैं तो के बारे में और अधिक गहराई से जानना होगा। आप अन्य कमांडों को पैटर्न से मेल खाने वाले अक्षरों को हटाने के लिए उपयोग कर सकते हैं। आप अपने स्क्रिप्ट में नीचे दिए गए लिंक्स से अधिक जानकारी पा सकते हैं।

## इसके अलावा देखें

- [Sed Command in Linux](https://www.geeksforgeeks.org/sed-command-in-linux-unix-with-examples/)
- [Using Sed in Scripts](https://www.linode.com/docs/tools-reference/tools/introduction-to-sed-commans/)
- [Fish Shell Documentation](https://fishshell.com/docs/current/tutorial.html)