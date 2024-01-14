---
title:    "Haskell: रेगुलर एक्सप्रेशंस का उपयोग करना"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## अभी क्यों लगाएं नियमित अभिव्यक्तियों का उपयोग करें?

नियमित अभिव्यक्तियों का उपयोग करना सबसे आसान तरीका है अपने डेटा को खोजने और उसे प्रसंस्करण करने का। यह उपकरण लगभग हर प्रोग्रामर को जानना चाहिए, क्योंकि यह समय और प्रयास दोनों बचाता है और पाठकों को अधिक उपयोगी बनाता है।

## कैसे करें:

जावास्क्रिप्ट में नियमित अभिव्यक्तियों का उपयोग करना आसान होता है। आइए हम हास्केल में भी इसका उपयोग करना सीखें:

```Haskell
import Text.Regex.Posix

-- नियमित अभिव्यक्ति की सहायता से अपने डेटा में खोज करें
-- मान लें, हमें एक दिनांक के साथ एक सीजन में बना संगीत का संग्रह है
-- हम चाहते हैं कि हम केवल गर्माहट, सी-शार्प की दर्जी एवं संगीत नाम भिन्न करें

songs = [(2018/09/12, "गर्माहट - F मज़ाकर", "सबसे अच्छे गाने"),
         (2018/09/14, "गर्माहट - फल इंडस्ट्री", "सबसे अच्छे गाने"),
         (2018/09/20, "सी-शार्प की दर्जी - ऑफिस रेकॉर्ड्स", "सबसे अच्छे गाने")]

-- फिल्टर के साथ नियमित अभिव्यक्ति का उपयोग करें
warmSongs = filter (\(_, _, name) -> name =~ ".*गर्माहट.*" ) songs
-- ["गर्माहट - F मज़ाकर", "गर्माहट - फल इंडस्ट्री"]

sharpSongs = filter (\(_, sharps, _) -> sharps =~ "सी-शार्प की दर्जी.*") songs
-- ["सी-शार्प की दर्जी - ऑफिस रेकॉर्ड्स"]

-- अब हमें यह भी देखना है कि क्या हम गाने के