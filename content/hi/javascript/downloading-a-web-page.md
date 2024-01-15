---
title:                "वेब पेज को डाउनलोड करें"
html_title:           "Javascript: वेब पेज को डाउनलोड करें"
simple_title:         "वेब पेज को डाउनलोड करें"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्यों

वेब पेज को डाउनलोड करना क्यों महत्वपूर्ण है? अपने कंप्यूटर पर फाइल को स्थानीय संग्रह में संचित करने से सुगमता के लिए, गूगल इमेज डाउनलोड करने से अपने स्क्रीनप्रिंट में प्रवेश करके और अपने पीसी पर अन्य सुविधाओं जैसे प्रमुख विस्तार या अड़चन को दूर करता है।


## कैसे करें

```javascript
// वेब पेज का URL निर्दिष्ट करें
let url = "https://www.example.com";

// नया XMLHttpRequest ऑब्जेक्ट बनाएं
let xhr = new XMLHttpRequest();

// "GET" आगंतुक का उपयोग करके अनुरोध सृजित करें
xhr.open("GET", url);

// प्रतिक्रिया निगरानी के लिए callback फ़ंक्शन निर्धारित करें
xhr.onload = function() {
  // कोड 200 वापस आने पर कुछ करें
  if (this.status === 200) {
    // उत्तर प्राप्त करें
    console.log("वेब पेज सफलतापूर्वक डाउनलोड किया गया।");
    // उत्तर को HTML में परिवर्तित करें
    let html = this.responseText;
    // अब आप उत्तर से ही जो भी करना चाहते हैं, कर सकते हैं।
  } else {
    // कोई त्रुटि हुई है
    console.log("त्रुटि हुई है।");
  }
}

// वेब पेज को डाउनलोड करें
xhr.send();
```

बाद में, अपने कोड को उन त्रुटियों से बचाने के लिए आप "try-catch" ब्लॉक का भी उपयोग कर सकते हैं।

## गहरी छानबीन

इस विषय में और गहरे जानने के लिए, आप अन्य XMLHttpRequest मेथड जैसे "POST", "PUT", "DELETE" और "HEAD" और इनके लिए उपयोगी HTTP स्टेटस कोड सीख सकते हैं। आप इस तरह एक्सेस कन्ट्रॉल, टाइमआउट, और कुल विन्यास जैसे विशिष्ट पैरामीटरो