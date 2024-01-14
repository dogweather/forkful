---
title:                "C#: एक http अनुरोध भेजना"
simple_title:         "एक http अनुरोध भेजना"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

# क्यों

अगर आप एक वेब डेवलपर हैं तो आपने शायद HTTP रिक्वेस्ट के बारे में सुना होगा। अलग-अलग वेब एप्लीकेशन इसे ध्यान से उपयोग करते हैं। हालांकि, यदि आपने अभी तक इसे सीखा नहीं है, तो आपको शायद इसका मूल्य नहीं पता हो। लेकिन HTTP रिक्वेस्ट का उपयोग करना आपको अपनी वेब एप्लीकेशन को सर्विस बनाने में बहुत मदद कर सकता है।

# कैसे

कर्सर का यह वेबसाइट अपने HTTP रिक्वेस्ट को बहुत सहजता से सभी वेबसर्विस द्वारा स्वीकार किया गया बॉडी में Json, ताकि आप यह पहली देख सकते कि आपके द्वारा प्रदत्त डेटा अपनी पीएजी रिक्वेस्ट और उससे उत्पन्न रिस्पॉन्स के साथ संबंधित वस्तु क्या हैं। उदाहरण के कैसे कोड ब्लॉक के साथ प्रदर्शित किया गया है:

```C#
using System;
using System.Net.Http;

namespace HttpSample
{
    class Program
    {
        static async Task Main(string[] args)
        {
            // URL सेट करें
            string url = "https://jsonplaceholder.typicode.com/todos/1";
            
            // HTTP क्लाइंट बनाएं 
            HttpClient client = new HttpClient();
            
            // रिक्वेस्ट बनाएं
            HttpRequestMessage request = new HttpRequestMessage();
            request.Method = HttpMethod.Get;
            request.RequestUri = new Uri(url);
            
            // रिस्पॉन्स प्राप्त करें
            HttpResponseMessage response = await client.SendAsync(request);
            
            // रिस्पॉन्स को पारस् करें
            string result = await response.Content.ReadAsStringAsync();
            Console.WriteLine(result);
        }
    }
}
```

आप यहां एक HTTP GET रिक्वेस्ट बना रहे हैं और उसे पहले रिस्पॉनस के रूप में पारस् कर रहे हैं। इससे आपको दिखेगा कि सर्वर क्या रिक्वेस्ट के लिए से जानकारी भेज रहा है।

# गहराई समीक्षा

HTTP रिक्वेस्ट भेजना शाय