---
title:                "Gleam: एक एचटीटीपी अनुरोध भेजना"
simple_title:         "एक एचटीटीपी अनुरोध भेजना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्यों

एचटीटीपी रिक्वेस्ट भेजने के लिए कोई काम क्यों करेगा।

यह एक अभ्यासों के अनुभव से समझते हैं कि प्रोग्रामर स्टार्टअप्स से प्रतिश्रूद्ध अपने ग्राहकों के लिए वेब एप्लिकेशन्स बनाने का काम नहीं करते, बल्कि वे अन्य कम्पनियों के साथ साझा काम कर सकते हैं और स्टार्टअप अपने डिजाइनरों से वेब डिजाइन का काम अपने उपयोगकर्ताओं से ढाचा इन्स्टाल कर सकते हैं।

## कैसे करें

```
Gleam.http.get("https://www.example.com")
|> Gleam.Result.get_or_else(Ok, "Request Failed")
|> IO.puts
```

यह उदाहरण सामान्य एम्पटवेटे प्रोग्रामिंग लैंग्वेजेज की तुलना में पारस्परिक अनुसूचनाओं (hierarchical logging) का उदाहरण है। response तो ok और imported दोनों आस (async) प्रोमिसस को backhaul करती हैं, और प्रत्येक प्रोमिस को क्लोन करता है।

## गहराई तक

एचटीटीपी रिक्वेस्ट भेजने का एक और तरीका है एचटीटीपी आईओ (HTTP IO) जिसमें हम कुछ और विशेषताओं के साथ इनफ़ोर्मेशन भेज सकते हैं जैसे कि टाइमआउट, प्रोक्सी, उपयोगकर्ता पासवर्ड, और आईओ लिमिट्स (IO limits)। आईओ के दोहन(ड्राइव के आटोटोलासिस AUTOTOLOUS जैसे प्रति सेकंड कितनी बार लैगाउट केंसल करें। अधिक जानकारी के लिए, आप [ग्लीम आईओ लाइवरी (HTTP IO library)](https://github.com/gleam-lang/http) की डॉक्यूमेंटेशन देख सकते हैं।

## देखें भी

- [ग्लीम आईओ लाइवरी (HTTP IO library)](https://github.com/gleam-lang