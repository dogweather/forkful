---
title:    "Gleam: टेक्स्ट खोज और प्रतिस्थापन करना"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्यों

व्यक्ति सामान्यतः लिखित कन्वर्शन से सामना करते हैं। इसके बारे में अधिक जानकारी के लिए, ये साधारण उदाहरणों को संलग्न करने के साथ समायोजित नहीं हैं, और ऐसे मुद्दे को उचित समाधान द्वारा हल किया जाता है।

## कैसे करे

अगर आप किसी पाठ में शब्द की खोज करना और उसे बदलना चाहते हैं, तो आप Gleam के साथ यह किसी भी आसानी से कर सकते हैं। नीचे दिए गए उदाहरण में, हम एक पाठ की "Hello World" स्ट्रिंग को "Namaste Gleam" में बदलने के लिए एक साधारण सेक्शन किया है।

```Gleam
let text = "Hello World"
let new_text = String.replace(text, "Hello", "Namaste")

io.println(new_text)
```

आउटपुट:

```Gleam
Namaste Gleam
```

## गहराई में जाएं

शब्द की खोज और बदलने का कार्य आंशिक स्ट्रिंग हिस्सों के साथ किया जा सकता है, लेकिन Gleam में आप एक उचित और संभावित स्थिति डिब्बे काउंट (captures) के साथ काम कर सकते हैं। आप इस फीचर को अपने स्क्रिप्ट में उपयोग करके उसके परिणाम को अभिन्न हिस्सों में वापस कर सकते हैं। नीचे जानें कि कैसे आप इस फीचर का उपयोग कर सकते हैं।

अपने स्क्रिप्ट में निम्नलिखित कोड डालें:

```Gleam
let regex = Regex.new("\\w+")
let text = "Hello, my name is Jane."

let result = Regex.replace_all(regex, text, "Molly")

io.println(result)
```

आउटपुट:

```Gleam
Molly, Molly Molly Molly Molly.
```

## इससे जुड़े और अधिक जानकारी के लिए

[Gleam Official Documentation](https://gleam.run/)

[Gleam Tutorial: Finding and Replacing Text](https://gleam.run/articles/tutorial-find-replace-text.html)
[Gleam String Module](https://gleam.run/modules/gleam/string.html)

[Using Regex in Gleam](https://gleam.run/articles/using-regex.html)