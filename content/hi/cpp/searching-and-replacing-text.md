---
title:                "C++: टेक्स्ट खोजना और बदलना"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

आपने कभी सोचा है कि आपने जब भी कोई लंबे टेक्स्ट को लिखने की कोशिश की हो, तो कभी-कभी आपको कुछ गलतियां भी हो जाती हैं? या फिर जब आपको बहुत सारे टेक्स्ट फाइलों में से एक विशेष शब्द या वाक्य को बदलना हो तो आपको कितना समय लगता है? यदि हाँ, तो आपको सॉफ्टवेयर डेवलपमेंट जगत में बहुत ही उपयोगी एक टेक्निक के बारे में पता होना चाहिए - टेक्स्ट को सर्च और रिप्लेस करना।

## क्यों

यह टेक्निक आपको टेक्स्ट को अतिरिक्त समय और मेहनत लगाए बिना सम्पादन करने की अनुमति देती है। आप बस कुछ सरल कोड द्वारा अपना लक्ष्य प्राप्त कर सकते हैं और टेक्स्ट के अनुकूलन कर सकते हैं, इससे आपका समय बचता है और आपको और अधिक सक्रिय बनाता है।

## कैसे करें

```C++
// सर्च और रिप्लेस करने के लिए आपको सिर्फ दो चीजें चाहिए - स्ट्रिंग और फाइल
string search = "विशेष शब्द";
string replace = "बदले गए शब्द";

// सभी टेक्स्ट फाइलों को धुनो
for (auto& file : filesystem::directory_iterator("फ़ोल्डर पथ")) {
    ifstream fin(file);
    string line;
    // टेक्स्ट फाइल को लाइन बाइ लाइन पढ़ो
    while (getline(fin, line)) {
        // स्ट्रिंग search को स्ट्रिंग replace से रिप्लेस करो
        for (size_t pos = 0; (pos = line.find(search, pos)) != string::npos; pos += replace.length()) {
            line.replace(pos, search.length(), replace);
        }
        // परिणाम को output.txt फाइल में लिखो
        ofstream fout("output.txt", ios::app);
        fout << line << endl;
        fout.close();
    }
}
```

जैसा कि आप ऊपर कोड में देख सकते हैं, आपको सिर्फ दो