---
title:                "डीबग आउटपुट प्रिंट करना"
html_title:           "Gleam: डीबग आउटपुट प्रिंट करना"
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

"डिबग आउटपुट" प्रिंट करना मतलब है कोड के विषम पहलुओं को समझने के लिए आवश्यक जानकारी दिखाने की क्रिया। प्रोग्रामर्स इसे करते हैं क्योंकि यह उन्हें बग की खोज और समस्या का समाधान करने में मदद करता है।

## कैसे:

Fish Shell में डिबग आउटपुट को मुद्रित करने के लिए, `echo` कमाँड का उपयोग करें।

```Fish Shell
function debug
    echo "Debug: $argv"
end
```

जब आप इसे `debug "Hello World"` के साथ कॉल करते हैं, तो यह "Debug: Hello World" मुद्रित करता है।

## गहरा डाइव

1) हिस्टॉरिकल कॉंटेक्स्ट: डिबग आउटपुट प्रिंट करना, प्रोग्राम की एक्सिक्यूशन फ्लो को समझने का एक प्राचीन तरीका है, जिसने अपनी सार्थकता यहां तक बनाई रखी है।

2) विकल्प: अन्य शेलल जैसे ki Bash, `echo` के विकल्प के रूप में `printf` का उपयोग कर सकते हैं।

3) विधान विवरण: Fish Shell में, `echo` कमाँड आर्ग्युमेंट को एक newline के साथ मुद्रित करता है। जेस्ट नियंत्रण संकेतम का इस्तेमाल करके आप न्यूलाइन को मिलाने का नियंत्रण कर सकते हैं। 

## अनुशासित:

- [Fish-Shell स्वीकार्य विचारनीय प्रक्रिया](https://fishshell.com/docs/current/tutorial.html)
- [Fish-Shell के साथ डिबगिंग](https://fishshell.com/docs/current/faq.html#faq-debugging) 