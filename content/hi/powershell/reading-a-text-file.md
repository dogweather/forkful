---
title:                "एक टेक्स्ट फ़ाइल पढ़ना"
html_title:           "PowerShell: एक टेक्स्ट फ़ाइल पढ़ना"
simple_title:         "एक टेक्स्ट फ़ाइल पढ़ना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
टेक्स्ट फ़ाइल को पढ़ना क्या है और क्यों प्रोग्रामर्स इसे करते हैं? एक टेक्स्ट फ़ाइल एक सामान्य टेक्स्ट डॉक्यूमेंट होती है जिसमें कई पंक्तियां हो सकती हैं जो किसी भी टेक्स्ट प्रोसेसिंग के लिए प्रयोग की जा सकती हैं। प्रोग्रामर्स इसे इसलिए करते हैं कि वे इसमें से डेटा को एक्सेस कर सकें और उसे अपनी प्रोग्राम के लिए उपयोग कर सकें।

## कैसे करें:
```PowerShell
# टेक्स्ट फ़ाइल को पढ़ने के लिए निम्न आदेश का प्रयोग करें।
Get-Content C:\textfile.txt
```

आप टेक्स्ट फ़ाइल के अनुसार अपनी जरूरत के अनुसार डेटा को अलग-अलग कर सकते हैं। निम्नलिखित आदेश विशेष रूप से प्रयोग कर सकते हैं:

```PowerShell
# स्ट्रिंग को औसत से अलग करने के लिए:
Get-Content C:\textfile.txt | Measure-Object -Average | Select-Object Average

# पाठ के आखरी पंक्ति को प्रिंट करने के लिए:
Get-content C:\textfile.txt | Select-Object -Last 1

# निश्चित स्थान से शुरू तक सेलेक्ट करने के लिए:
Get-Content C:\textfile.txt | Select-Object -Skip 2 -First 5
```

## गहराई में उतरें:
(1) टेक्स्ट फ़ाइलों ने प्रोग्रामिंग के शुरुआती दिनों से एक प्रमुख भूमिका निभाई है। ये गैर-मेमोरी इंस्ट्रुमेंट्स होती हैं जो प्रोग्रामर्स को उनके विभिन्न सिस्टम्स पर समान डेटा एक्सेस का अनुमान करने में मदद करती हैं। (2) अन्य विकल्प जैसे CSV फ़ाइलें भी प्रोग्रामर्स द्वारा उपयोग किये जाते हैं, लेकिन वे संरचनाओं को बंद द्वारा समर्थित नहीं करते हैं जैसे विभिन्न पंक्तियों को उपयोग किए बिना संग्रह नहीं कर पाते हैं। (3) टेक्स्ट फ़ाइलों को पढ़ने का आसान और मज़ेदार तरीका इसके PowerShell में अधिकतम उपयोग दिखाता है।

## इससे जुड़िये:
- [Microsoft Docs - Get-Content](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content?view=powershell-7)
- [Geeks for Geeks - Reading a Text File using PowerShell](https://www.geeksforgeeks.org/reading-a-text-file-using-powershell/)
- [PowerShell Scripting Tutorial for Beginners](https://www.tutorialspoint.com/powershell/index.htm)