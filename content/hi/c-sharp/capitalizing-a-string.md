---
title:                "एक स्ट्रिंग को कैपिटलाइज़ करना"
html_title:           "C#: एक स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "एक स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# क्या और क्यों?

एक स्ट्रिंग को कैपिटलाइज करना मतलब उसमें हर शब्द के पहले अक्षर को बड़ा लिखना है। यह एक प्रोग्रामिंग टेक्निक है जिससे विशेष शब्दों पर ध्यान केंद्रित किया जाता है और लोग अपनी कोडिंग रिपोर्ट्स या लोगों को प्रभावी विश्वव्यापी स्ट्रिंग्स बनाते हैं।

# कैसे करें:

``` C#
string sentence = "hello world";
string capitalizedSentence = sentence.ToUpper();
Console.WriteLine(capitalizedSentence);
```

आउटपुट:
HELLO WORLD

```C#
string sentence = "hello world";
string capitalizedSentence = CultureInfo.CurrentCulture.TextInfo.ToTitleCase(sentence);
Console.WriteLine(capitalizedSentence);
```

आउटपुट:
Hello World

# गहराई का खोज

इस टेक्निक को लागू करने का इतिहास वास्तव में स्ट्रिंग्स पर काम करने का ही है। यह कई प्रायोगिक उदाहरण भी है जैसे कि प्रोग्रामिंग में कोडिंग रिपोर्ट्स और भाषाई टेक्स्ट को प्रभावी करने के लिए। इस टेक्निक का एक विकल्प है शब्द तरह का प्रसिद्धी। इसमें स्ट्रिंग्स को विशेष शब्दों पर बड़े अक्षरों या विशेष चिह्नों के साथ प्रस्तुत किया जाता है। इसके अलावा, कुछ प्रोग्रामिंग भाषाओं में यह कैपिटलाइज करने के लिए विशेष फंक्शन भी होते हैं।

# भी देखें:

https://www.c-sharpcorner.com/article/capitalize-a-string-with-C-Sharp/
https://www.geeksforgeeks.org/capitalize-first-letter-string-java/
https://www.tutorialspoint.com/uppercase-and-lowercase-string-characters-in-csharp