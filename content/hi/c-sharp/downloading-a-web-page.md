---
title:                "वेब पेज डाउनलोड करना"
html_title:           "C#: वेब पेज डाउनलोड करना"
simple_title:         "वेब पेज डाउनलोड करना"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
वेब पृष्ठ डाउनलोड करना क्या है और सॉफ्टवेयर डेवलपर इसे क्यों करते हैं, इसकी संक्षिप्त व्याख्या।
 
## कैसे करें:
```C#
// यूआरएल स्ट्रिंग से वेब पृष्ठ डाउनलोड करें
string url = "https://www.example.com";
using (WebClient client = new WebClient()) 
{
    string html = client.DownloadString(url);
    Console.WriteLine(html);
}
```

## गहराई में जाएं:
- इतिहास: वेब पृष्ठ डाउनलोडिंग के पीछे का ऐतिहासिक संदर्भ।
- विकल्प: अन्य तरीके जिनसे आप वेब पृष्ठ डाउनलोड कर सकते हैं।
- अंतर्निर्माण विवरण: वेब पृष्ठ डाउनलोडिंग का अंतर्निर्माण और दूसरे आवश्यक चरण।

## और देखें:
संबंधित स्रोतों के लिंक।