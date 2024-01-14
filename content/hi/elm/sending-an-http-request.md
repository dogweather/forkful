---
title:                "Elm: एक HTTP अनुरोध भेजना"
simple_title:         "एक HTTP अनुरोध भेजना"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्यों

एक Http अनुरोध भेजने में शामिल होने के लिए *क्यों* कोई इच्छुक होगा? Http अनुरोध भेजने के माध्यम से, हम सर्वर से डेटा को आसानी से प्राप्त कर सकते हैं और ऐप्स या वेबसाइटों के बीच डेटा को शेयर कर सकते हैं। यह डेटा लेने या भेजने का सबसे आसान तरीका है और Elm में भी अत्यंत सरल है।

## कैसे करें

एल्म में Http अनुरोध भेजना आसान है। हम `Http.send` फ़ंक्शन का उपयोग कर सकते हैं जो दो विशिष्ट पैरामीटर देती हैं - प्रथम `Config` और दूसरा `Msg`। `Config` में, हम अपने अनुरोध का प्रकार (`GET`, `POST`, `PUT` आदि) और अनुरोध का URL देते हैं। `Msg` में, हम अपने अनुरोध का परिणाम प्राप्त करने के लिए एलम संदेश निर्धारित करते हैं।

एक उदाहरण के लिए, हम अब्ज़र्वेबल.आरटीएनईके वेबसाइट से एक ब्लॉग पोस्ट का शीर्षक लेने के लिए यह कोड लिख सकते हैं:

```
Elm.Http.send getBlogTitle (Elm.Http.request "https://observablehq.com/@rtnee/blog")
  |> Task.perform (\_ ->
    Debug.log "Error" "Unable to retrieve blog title"
  )

getBlogTitle : Result Http.Error String -> Msg
getBlogTitle result =
  case result of
    Ok response ->
      case response.body of
        Ok title ->
          SetTitle title
        Err _ ->
          SetTitle "No title found"
    Err error ->
      SetTitle "No title found"
```

जब यह कोड चलता है, तो यह शीर्षक "civilmoney और एलम आरटीएनई" निकलेगा। अगर कोई त्रुटि होती है, तो "शीर्षक नहीं मिला" निकलेगा।

## गहराई में जाएं

एलम में Http अनुरोध बेहद आसान हैं लेकिन आपको कुछ गहराई के साथ चलने की आवश्यकता हो सकती है। आप क्षेत