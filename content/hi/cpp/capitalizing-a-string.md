---
title:                "स्ट्रिंग को अपरकेस में बदलना"
html_title:           "C++: स्ट्रिंग को अपरकेस में बदलना"
simple_title:         "स्ट्रिंग को अपरकेस में बदलना"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्यों
कोई भी एक रूचि वाला या प्रोग्रामिंग शुरुआती उपयोगकर्ता एक स्ट्रिंग को कैपिटलाइज़ करने के माध्यम से उसके फायदे को अधिक जानने के लिए उनके स्ट्रिंग मैनिपुलेशन कौशल को बढ़ाने के लिए इस आलेख की तलाश में हो सकते हैं।

## कैसे करें
यदि आपके पास एक संचिका में कुछ संहिता है और आप उसे लिखना चाहते हैं, तो उसे कैपिटलाइज़ करना भी बहुत आसान हो सकता है। यहां हम आपको उस सॉल्यूशन की एक संख्या बताएंगे जो आपकी समस्या को हल कर सकती है। यह एक संदर्भ पाठ है और आपको तक गठरी लिखने की आवश्यकता होगी।

```C++
#include <iostream>
#include <string>
using namespace std;

// Function to capitalize a string
void capitalize(string& s)
{
    // Loop through each character
    for (int i = 0; i < s.length(); i++)
    {
        // Check if the character is lowercase
        if (s[i] >= 'a' && s[i] <= 'z')
        {
            // Convert it to uppercase
            s[i] = toupper(s[i]);
        }
    }
}

int main()
{
    // Input string from user
    string str;
    cout << "Enter a string: ";
    getline(cin, str);

    // Call the capitalize function
    capitalize(str);

    // Print the capitalized string
    cout << "Capitalized string: " << str;

    return 0;
}
```

उपरोक्त कोड को एक आईडिया के माध्यम से, हमें आप के मामले में जिन शब्दों को लिखने होंगे, उन्हें प्रवर्धित करने की आवश्यकता होती है, वे सभी शब्द शुरूआत में बहुत साधारण होते हैं, तो आपको केवल अपने स्ट्रिंग नाम का उपयोग करना होगा। यह स्पष्टीकरण का काम काफी आसान है।

## गहराई से जानना
स्ट्रिंग कैपिटलाइज़ को संभव बनाने के लिए सटीक रूप से हमारे अ्दारा उपयोग किए