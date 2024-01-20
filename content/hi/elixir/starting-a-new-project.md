---
title:                "नया प्रोजेक्ट शुरू करना"
html_title:           "C: नया प्रोजेक्ट शुरू करना"
simple_title:         "नया प्रोजेक्ट शुरू करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

# नई प्रोजेक्ट: आरंभ करने की विधि - Elixir Programming (वर्तमान संस्करण)

## क्या और क्यों?

नई प्रोजेक्ट का आरंभ करना मतलब एक नया कोडिंग कार्य शुरू करना है। प्रोग्रामर इसे इसलिए करते हैं ताकि वे अपनी योजनाएं या सिस्टम का स्केलअप कर सकें। 

## कैसे:

Elixir में नई प्रोजेक्ट आरंभ करने के लिए देखिए:

```elixir
mix new project_name
```

जैसे कि, हमारी प्रोजेक्ट "नमस्ते_दुनिया" है, तो हमें निम्नलिखित करना होगा:

```elixir
mix new नमस्ते_दुनिया
```

## गहरी जानकारी

1. ऐतिहासिक संदर्भ: Elixir, एरिक संदग्रेन द्वारा डिजाइन और विकसित किया गया, ऑपन-सोर्स, सामर्थ्य-केंद्रित, सामान्य प्रयोजन की भाषा है, जिसका उद्देश्य तर्क का निर्माण करना, वितरण, और त्रुटियों को ठीक करना है। 

2. विकल्प: नई प्रोजेक्ट को आरंभ करने के लिए अन्य भाषाएं जैसे कि Ruby, Python, Java इत्यादि भी इस्तेमाल की जा सकती हैं। लेकिन Elixir इसे बहुत सरल और आसान बनाता है। 

3. कार्यान्वयन विवरण: ऊपर दिए गए `mix new` आदेश से Elixir नई प्रोजेक्ट डायरेक्ट्री बना देता है, `lib` डायरेक्ट्री में कोडींग के लिए खाली फ़ाइलें बनाता है, और `test` डायरेक्ट्री में खाली टेस्ट फ़ाइलें बनाता है। 

## अतिरिक्त जानकारी

1. [Elixir का आधिकारिक डॉक्युमेंटेशन](https://elixir-lang.org/docs.html)
2. [Elixir का गेटिंग स्टार्टेड गाइड](https://elixir-lang.org/getting-started/introduction.html)
3. [Elixir के साथ प्रोजेक्ट की बुनियादी संरचना कैसे सेट करें](https://elixirschool.com/en/lessons/basics/mix/)
4. [Elixir प्रोजेक्ट संगठन: टिप्स और ट्रिक्स](https://medium.com/@ttacoopes/structuring-elixir-projects-d91d7951b185)