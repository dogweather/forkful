---
title:    "Elixir: पाठ खोजना और बदलना"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# क्यों
टेक्स्ट को खोजने और बदलने में जुटने का कारण केवल 1-2 वाक्यों में बताया गया है।

आपने अपने लैपटॉप में वह फॉल्डर खोजा है और आपको वहां कई फाइलें मिलीं हैं जिनमें कुछ समान शब्द शामिल हैं। अब आपको उन शब्दों को एक से ज्यादा फाइलों में बदलना होगा। आखिर में, आपके लिए समय की बचत और परिणाम को सुनिश्चित करने के लिए, सर्च और रिप्लेस फंक्शन का इस्तेमाल सरल और अच्छा तरीके है।

# कैसे
आप अपने Elixir प्रोग्राम में सर्च और रिप्लेस फंक्शन को कैसे इस्तेमाल कर सकते हैं, इसका डेमो निम्न सहायता से समझिये।

```Elixir
my_string = "मेरा नाम जॉन है। मुझे प्रोग्रामिंग पसंद है।"
updated_string = String.replace(my_string, "पसंद", "लोभ")

IO.puts updated_string # मेरा नाम जॉन है। मुझे प्रोग्रामिंग लोभ है।
```

इस उदाहरण में, हमने "मेरा नाम जॉन है। मुझे प्रोग्रामिंग पसंद है।" जैसे स्ट्रिंग में "पसंद" को "लोभ" से बदल दिया है। सर्च और रिप्लेस फंक्शन स्ट्रिंग के भीतर उपलब्ध सभी आइटम्स का खोज करता है और उन्हें बदल देता है। आप इस तरीके से बड़ी से बड़ी स्ट्रिंग को आसानी से बदल सकते हैं।

# गहराई तक जाएं
अब आपने देखा है कि सर्च और रिप्लेस फंक्शन को कैसे काम किया जाता है, लेकिन आपको इसके बारे में और गहराई से