---
title:                "बेसिक प्रमाणीकरण के साथ एचटीटीपी अनुरोध भेजना"
html_title:           "Elm: बेसिक प्रमाणीकरण के साथ एचटीटीपी अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ एचटीटीपी अनुरोध भेजना"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
बुनियादी प्रमाणिकरण के साथ एचटीटीपी अनुरोध भेजना क्या है और प्रोग्रामर्ज यह क्यों करते हैं।

## कैसे:
एल्म उदाहरण बनाने के लिए कोडिंग और नमूना आउटपुट को ```Elm...``` कोड ब्लॉक के अंदर दिखाया गया है ।

```Elm
--एचटीटीपी अनुरोध भेजने का एक उदाहरण

--आईपी और यूजरनेम की जानकारी सहित एचटीटीपी URL तैयार करें
url : String
url =
  "http://example.com/api/users"

--क्रेडेंशियल्स और हेडर जोड़ें
request =
  { method = "GET"
  , headers = [("Authorization", "Basic YWRtaW46cGFzc3dvcmQ=")]
  }

--असिंक्रोनस एचटीटीपी अनुरोध भेजें और प्रतिक्रिया को हैंडल करें
Http.send handleResponse (Http.request { method = "GET", url = url, headers = [("Authorization", "Basic YWRtaW46cGFzc3dvcmQ=")], expect = Http.expectJson identity })

--अनुरोध सफल होने पर उत्तर प्राप्त करें
handleResponse : Result Http.Error (List User)
handleResponse result =
  case result of
    Ok users ->
      --उत्तर में सफलतापूर्वक एचटीटीपी अनुरोध का प्रतिबोधन करें
      Debug.log "Successfully sent HTTP request!" users
    Err error ->
      --अनुरोध विफल होने पर त्रुटि को हैंडल करें
      Debug.log "Error sending HTTP request!" error
```

## गहराई में जाएं:

एचटीटीपी प्रमाणीकरण के साथ एचटीटीपी अनुरोध भेजने का इतिहास, वैकल्पिक विचार और संचालन विवरण के बारे में अधिक जानकारी।

1) इतिहास - एचटीटीपी प्रमाणीकरण का उपयोग मूल रूप से उपभोक्ताओं का प्रोफाइल और पासवर्ड को एचटीटीपी अनुरोध में सहायक प्रकार से साथ भेजने के लिए होता था। यह अभी भी आधुनिक एचटीटीपी अनुरोध में सर्वाधिक प्रयोग किया जाने वाला प्रकार है।

2) वैकल्पिक विचार - अन्य प्रमाणीकरण विधियां जैसे OAuth भी एचटीटीपी अनुरोध के साथ उपयोग की जा सकती है। यह सुरक्षित और स्थायी होने के साथ साथ, लोगिन को आसान बनाने में मदद करता है।

3) संचालन विवरण - एचटीटीपी अनुरोध भेजने के लिए, उपभोक्ता के क्रेडेंशियल्स को अनुरोध हेडर में Base64 encoded रूप में जोड़ा जाता है। सर्वर प्रोग्राम में, इस अनुरोध से प्राप्त उपयोगकर्ता नाम और पासवर्ड को सत्यापित की जाती है।

## इससे जुड़ी देखें:
एस्टेजमेंट में उपयोयी विवरण, उदाह