---
title:                "C#: बेसिक ऑथेंटिकेशन के साथ एक एचटीटीपी अनुरोध भेजना।"
simple_title:         "बेसिक ऑथेंटिकेशन के साथ एक एचटीटीपी अनुरोध भेजना।"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# क्यों?

HTTP अनुरोध को बेसिक प्रमाणीकरण के साथ भेजने में लोगों को क्यों रुचि हो सकती है?

बेसिक प्रमाणीकरण एक सरल प्रमाणीकरण प्रणाली है जिसे प्रयोगकर्ताओं को अपने बाहरी संसाधनों का प्रबंधन करने की अनुमति देता है। इसमें प्रयोगकर्ता एक उपयोगकर्ता नाम और पासवर्ड भरकर निदेशित अनुरोध भेजता है। इसका प्रयोग उन वेब साइटों पर होता है जहां उपयोगकर्ताओं के लिए उपयुक्त संसाधनों तक पहुंच को नियंत्रित किया जाता है, जैसे सम्पादन के लिए उपयुक्त संसाधन या आरंभिक सत्र संपादन का प्रबंधन।

# कैसे करें?

```C#
// एक HTTP अनुरोध भेजें जो बेसिक प्रमाणीकरण का प्रयोग करता है
var request = (HttpWebRequest)WebRequest.Create("http://example.com/resource");
request.Method = "GET";

// उपयोगकर्ता नाम और पासवर्ड निर्धारित करें
string username = "john";
string password = "12345";

// बेसिक प्रमाणीकरण उपयोग के लिए चेक मान सेट करें
request.Headers["Authorization"] = "Basic " + Convert.ToBase64String(Encoding.Default.GetBytes(username + ":" + password));

// अनुरोध का प्रतिक्रिया प्राप्त करें
var response = (HttpWebResponse)request.GetResponse();

// प्रतिक्रिया को पाठ के साथ प्रिंट करें
Console.WriteLine(new StreamReader(response.GetResponseStream()).ReadToEnd());
```

उपरोक्त कोड स्निपेट के द्वारा आप एक HTTP अनुरोध भेज सकते हैं जिसमें उपयोगकर्ता नाम और पासवर्ड के लिए बेसिक प्रमाणीकरण का प्रयोग होता है। अनुरोध को प्राप्त हुई प्रतिक्रिया को प्रिंट किया जाता है जो आपको पुन: पाठ के साथ स्ट्रीम करता है।

# गहराई मे जाएं

बेसिक प्रमाणीकर