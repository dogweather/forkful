---
title:                "टेस्ट लिखना"
aliases:
- /hi/bash/writing-tests.md
date:                  2024-02-03T19:30:16.849892-07:00
model:                 gpt-4-0125-preview
simple_title:         "टेस्ट लिखना"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
बैश में परीक्षण लिखना आपके बैश स्क्रिप्ट्स की कार्यक्षमता को वैलिडेट करने के लिए परीक्षण मामलों की स्क्रिप्टिंग को शामिल करता है। प्रोग्रामर विभिन्न परिस्थितियों में उनकी स्क्रिप्ट्स के अनुरूप काम करने की सुनिश्चितता के लिए परीक्षण करते हैं, तैनाती से पहले त्रुटियों और बग्स को पकड़ते हैं।

## कैसे करें:
बैश में कोई निर्मित परीक्षण फ्रेमवर्क नहीं है, लेकिन आप साधारण परीक्षण फंक्शन लिख सकते हैं। अधिक उन्नत परीक्षण के लिए, `bats-core` जैसे तृतीय-पक्ष उपकरण लोकप्रिय हैं।

### शुद्ध बैश में बुनियादी परीक्षण उदाहरण:
```bash
function test_example_function {
  result=$(your_function 'test_input')
  expected_output="expected_output"
  
  if [[ "$result" == "$expected_output" ]]; then
    echo "परीक्षण सफल."
    return 0
  else
    echo "परीक्षण विफल. '$expected_output' की उम्मीद थी, मिला '$result'"
    return 1
  fi
}

# परीक्षण फंक्शन को आह्वानित करना
test_example_function
```
नमूना आउटपुट:
```
परीक्षण सफल.
```

### परीक्षण के लिए `bats-core` का उपयोग करना:
पहले, `bats-core` को स्थापित करें। यह आमतौर पर आपके पैकेज मैनेजर के माध्यम से या इसकी रिपॉजिटरी को क्लोन करके किया जा सकता है।

फिर, अलग `.bats` फाइलों में अपने परीक्षण लिखें।

```bash
# फ़ाइल: example_function.bats

#!/usr/bin/env bats

@test "उदाहरण फ़ंक्शन को परीक्षित करें" {
  result="$(your_function 'test_input')"
  expected_output="expected_output"
  
  [ "$result" == "$expected_output" ]
}
```
अपने परीक्षणों को चलाने के लिए, केवल `.bats` फाइल को निष्पादित करें:
```bash
bats example_function.bats
```
नमूना आउटपुट:
```
 ✓ उदाहरण फ़ंक्शन को परीक्षित करें

1 परीक्षण, 0 विफलताएँ
```

यह दृष्टिकोण आपको अपनी विकास प्रक्रिया में परीक्षण को आसानी से एकीकृत करने की अनुमति देता है, आपके बैश स्क्रिप्ट की विश्वसनीयता और स्थिरता सुनिश्चित करता है।
