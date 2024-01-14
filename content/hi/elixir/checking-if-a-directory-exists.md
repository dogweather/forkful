---
title:                "Elixir: डायरेक्टरी की मौजूदगी की जांच"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्यों

एक डायरेक्टरी का अगर मौजूद होना जांचने में क्या फायदा है, यह पता लगाना महत्वपूर्ण है।

## कैसे करें

```Elixir
# यहाँ हम डायरेक्टरी का मौजूद होना जांचते हैं और उसकी कमांड लाइन में आउटपुट प्रिंट करते हैं।
dir_name = "./test_directory"
if File.dir?(dir_name) do
  IO.puts "डायरेक्टरी मौजूद है"
else
  IO.puts "डायरेक्टरी मौजूद नहीं है"
end
```

परिणाम:

```
डायरेक्टरी मौजूद है
```

## गहराई में

जब आप अपनी ऐप्लिकेशन में फाइल या फोल्डर को ऑपरेट करते हैं, तो आपको अनुकूलन के लिए उनमें से एक को आवश्यकता हो सकती है। इस लेख में हम डायरेक्टरी का मौजूद होना जांचने के लिए अलग-अलग तरीके देखेंगे और किसी फ़ोल्डर में फ़ाइलें खोजने के लिए भी इसका उपयोग कर सकते हैं।

## इस से सम्बंधित

अधिक जानकारी के लिए, नीचे दिए गए लिंक्स का उपयोग करें:

- [इरेलिक्स डॉक्युमेंटेशन](https://elixir-lang.org/getting-started/introduction.html)
- [डायरेक्टरी मौजूदिए कैसे जांचें पोस्ट](https://www.phoenixframework.org/blog/working-with-directories-in-elixir-and-phoenix)
- [ऐप्स में फोल्डर और फाइलें व्यवस्थित करने के लिए इरेलिक्स](https://akoutmos.com/post/working-with-directories-and-files-in-elixir/)
- [डायरेक्टरी आंकड़े और गुणिती](https://stackoverflow.com/questions/40991543/how-to-check-if-a-directory-exists-in-elixir)