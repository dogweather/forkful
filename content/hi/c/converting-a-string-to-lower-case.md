---
title:                "स्ट्रिंग को लोअर केस में बदलना"
html_title:           "C: स्ट्रिंग को लोअर केस में बदलना"
simple_title:         "स्ट्रिंग को लोअर केस में बदलना"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

जिस तरह से एकाधिक कार्य हमारे समझ और इस्तेमाल को आसान बनाने के लिए समान या समानित होते हैं, वैसे ही, भूमिका नेमे परिवर्तित की जाती है हमेशा एक स्ट्रिंग को निम्न अक्षरों में बदलना हर श्रेणी के सॉफ्टवेयर विकासक (programmer) के लिए अति प्राथ्मिक और जरूरी होता है. अब आप आसानी से यह समझ सकते हैं कि ऐसा क्यों होता है और इसका कैसे इस्तेमाल किया जाता है.

## What & Why?

शायद आपने कभी C लैंग्वेज में वर्किंग किया हो और तब आपको caps lock लॉक होने से सावधानी से इर्द-गिर्द वाले वर्ड्स में कुछ कन्फ्यूहोन हो सकती है. बस उसी तरह, कोडिंग मे ऐसा कभी-कभी हो सकता है. ऐसा हो जाने पर हमारा प्रोग्राम काम नहीं करता है. जिस समय हम अपने वर्क को debug करते हैं तो हम इस पर ध्यान देते हैं कि कहीं पर कोई त्रुटि तो नहीं हो गई है और उसे सही करने के लिये बहुत समय लगा सकता है. इसीलिये, हमें सिर्फ caps lock चीक करने के लिये ही वर्ड्स को स्ट्रिंग में लेना चाहिए ताकि हम अपनी प्रोग्राम में सभी गलतियों को पहले से ध्यान रख सकें.

## How to:

फेर आपको यह समझाते है कि कैसे आप आसानी से अपने प्रोग्राम में स्ट्रिंग को लोअर केस में रूपांतरित कर सकते हैं. नीचे कुछ कोडिंग उदाहरण हैं जो कि असल में आपको अपनी प्रोग्राम में इस्तेमाल करने हैं.

```C
#include <stdio.h>
#include <string.h>

int main()
{
    char string[20];
    printf("Enter a string: ");
    scanf("%s", string);
    for (int i = 0; i < strlen(string); i++)
    {
        string[i] = tolower(string[i]);
    }
    printf("The string in lower case is: %s", string);
    return 0;
}
```

अब आपको यह समझना है कि हमने जो स्ट्रिंग इनपुट लिया उसमें से एक नया स्ट्रिंग बनाया और उसे लोअर केस में लिखा है. लेकिन यह code आपको समझने में काफी टाइम लगा होगा.

सबसे अच्छी बात है कि हम `tolower` function की मदद से इसको बहुत सरल बना सकते हैं, जैसे कि नीचे दिए गए code में दिखाया गया है.

```C
#include <stdio.h>
#include <string.h>

void to_lower(char *string)
{
    int i = 0;
    while (string[i] != '\0')
    {
        if (string[i] >= 'A' && string[i] <= 'Z')
        {
            string[i] += 'a' - 'A';
        }
        i++;
    }
}

int main()
{
    char string[20];
    printf("Enter a string: ");
    scanf("%s", string);
    to_lower(string);
    printf("The string in lower case is: %s", string);
    return 0;
}
```

## Deep Dive:

पिछले कई वर्षों से संभवत